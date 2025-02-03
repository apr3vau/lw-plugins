;; Copyright (c) 2025, April & May
;; SPDX-License-Identifier: 0BSD

;; Pure-Lisp SVG renderer for LispWorks

;; The source code can be separated to - major parts, splitted with #\Page
;; 1. CSS parser
;; 2. SVG `path` data parser
;; 3. Gradient painting server implementation
;; 4. Main SVG parser & renderer

;; Check README.md for usage and details.

(defpackage lw-svg
  (:use #:string-case)
  (:import-from #:alexandria #:clamp #:copy-hash-table #:lerp #:ensure-gethash)
  (:import-from #:uiop #:string-prefix-p)
  (:import-from #:color #:make-rgb #:make-hsv #:ensure-rgb #:color-red #:color-green #:color-blue #:color-alpha)
  (:import-from
   #:gp
   #:make-transform #:premultiply-transforms #:postmultiply-transforms
   #:apply-scale #:apply-translation #:apply-rotation #:apply-rotation-around-point
   #:2pi #:pi-by-2 #:rectangle-bind)
  (:export
   rad-to-deg deg-to-rad hex-to-spec
   css-parse-url css-parse-angel css-parse-color css-parse-length
   css-parse-a-number css-parse-transforms css-parse-numeric-color css-parse-all-angels-from-string
   css-parse-all-length-from-string css-parse-all-numbers-from-string
   create-renderer draw-svg-from-string)
  (:add-use-defaults))

(in-package lw-svg)

;; Utils

(declaim (type double-float rad-to-deg-conversion-factor))

(defconstant rad-to-deg-conversion-factor (/ 180.0d0 pi)
  "Factor used to convert radiants to degrees by multiplication.")

(defun rad-to-deg (radians) (* radians rad-to-deg-conversion-factor))

(defun deg-to-rad (degree) (/ degree rad-to-deg-conversion-factor))

(defun hex-to-spec (hex)
  "Convert hexdecimal color string to LW color-spec."
  (setq hex (string-trim '(#\# #\Space) hex))
  (let ((hex-list (case (length hex)
                    (3 (map 'list #'string hex))
                    (6 (loop for i from 0 to 4 by 2 collect (subseq hex i (+ 2 i))))))
        (deno (if (< (length hex) 6) 15.0 255.0)))
    (when hex-list
      (apply #'make-rgb
             (mapcar (lambda (str) (/ (parse-integer str :radix 16) deno))
                     hex-list)))))

;; Why I'm prefer using CL's convention `NFOO` but not Scheme's `FOO!` XD...
(defun nmerge-tables (table &rest tables)
  "Merge values of hash-tables in TABLES into TABLE. TABLE will be modified.

From serapeum's `merge-tables!`"
  (declare (optimize (speed 3)))
  (reduce (lambda (ht1 ht2)
            (maphash (lambda (k v)
                       (setf (gethash k ht1) v))
                     ht2)
            ht1)
          tables
          :initial-value table))


;; (Partial) CSS parser

(defun css-parse-a-number (str &optional (start 0))
  (declare (type vector str) (type fixnum start)
           ;(:explain :types)
           )
  "Parsing a CSS number gracefully out of STR, starting from START.

Return the first number it met, and the end position of this number.
Return NIL if there isn't a number at START."
  ;; This function will be used heavily, so should be as fast as it can...
  (check-type str string)
  (let ((char-arr (make-array 8 :element-type 'character :fill-pointer 0 :adjustable t))
        (i start)
        (len (length str))
        has-dot
        has-exp)
    (declare (type vector char-arr exp)
             (type fixnum i len)
             (type boolean has-dot has-exp))
    (tagbody
     start
     (if (= i len) (go end))
     (let ((c (char str i)))
       (cond ((member c '(#\+ #\-))
              (if (= i start)
                (vector-push-extend c char-arr)
                (go end)))
             ((char= c #\.)
              (if has-dot (go end)
                (progn
                  (setq has-dot t)
                  (vector-push-extend c char-arr))))
             ((char-equal c #\e)
              (if has-exp (go end)
                (progn
                  (setq has-exp t)
                  (vector-push-extend c char-arr))))
             ((digit-char-p c)
              (vector-push-extend c char-arr))
             (t (go end))))
     (setq i (1+ i))
     (go start)
     end)
    (if (find-if #'digit-char-p char-arr)
      (values (parse-float char-arr :default-format 'double-float) i)
      nil)))

(defun css-parse-all-numbers-from-string (str)
  "Parse all CSS format numbers in STR, return as a vector."
  (declare (optimize (safety 0))
           (type string str))
  (let ((i 0)
        (len (length str))
        (numbers (make-array 15 :element-type 'double-float :fill-pointer 0 :adjustable t)))
    (declare (type fixnum i len))
    (tagbody
     start
     (if (= i len) (go end) nil)
     (let ((c (char str i)))
       (if (or (digit-char-p c) (member c '(#\+ #\-)))
         (multiple-value-bind (num idx)
             (css-parse-a-number str i)
           (if num
             (progn
               (vector-push-extend num numbers)
               (setq i idx)
               (go start))
             (go end)))
         (progn
           (setq i (1+ i))
           (go start))))
     end)
    numbers))

;; CSS length-percentage

(defun css-parse-length (port str &optional (width-or-height :width) viewport-w viewport-h parent-w parent-h)
  "Parse a CSS format <length-percentage> to corresponding pixels,
based on current graphics port, CSS viewport and element's parent."
  ;; FIXME: Not fully tested
  (capi:with-geometry port
    (unless viewport-w (setq viewport-w capi:%width%))
    (unless viewport-h (setq viewport-h capi:%height%))
    (unless parent-w (setq parent-w viewport-w))
    (unless parent-h (setq parent-h viewport-h))
    (when str
      (if (alpha-char-p (char str 0)) 0
        (multiple-value-bind (len len-end) (css-parse-a-number str)
          (declare (type fixnum len-end viewport-w viewport-h parent-w parent-h)
                   (type double-float len))
          (let* ((unit (subseq str len-end))
                 (font (capi:simple-pane-font port))
                 (size (gp:font-description-attribute-value (gp:font-description font) :size)))
            (* len
               (string-case (unit)
                 ;; abs https://www.w3.org/TR/css-values/#absolute-lengths
                 ("cm"   37.79527559055118D0) ;(/ 96d0 2.54d0)
                 ("mm"   3.7795275590551185D0) ;(/ 96d0 25.4d0)
                 ("Q"    0.9448818897637794D0) ;(/ 96d0 2.54d0 40d0)
                 ("in"   96d0)
                 ("pt"   1.3333333333333333D0) ;(coerce 4/3 'double-float)
                 ("pc"   16d0)
                 ("px"   1d0)
                 ;; rel https://www.w3.org/TR/css-values/#relative-lengths
                 ("em"   size)
                 ("rem"  size)
                 ("ex"   (+ (gp:get-char-ascent port #\x font) (gp:get-char-descent port #\x font)))
                 ("rex"  (+ (gp:get-char-ascent port #\x font) (gp:get-char-descent port #\x font)))
                 ("cap"  (+ (gp:get-char-ascent port #\O font) (gp:get-char-descent port #\O font)))
                 ("ch"   (gp:get-char-width port #\0 font))
                 ("rch"  (gp:get-char-width port #\0 font))
                 ("ic"   (gp:get-char-width port #\Ideographic-space font))
                 ("ric"  (gp:get-char-width port #\Ideographic-space font))
                 ("lh"   (gp:get-font-height port))
                 ("rlh"  (gp:get-font-height port))
                 ("vw"   (/ viewport-w 100d0))
                 ("vi"   (/ viewport-w 100d0))
                 ("vh"   (/ viewport-h 100d0))
                 ("vb"   (/ viewport-h 100d0))
                 ("vmin" (min (/ viewport-w 100d0) (/ viewport-h 100d0)))
                 ("vmax" (max (/ viewport-w 100d0) (/ viewport-h 100d0)))
                 ("%"    (if (eq width-or-height :width)
                           (/ parent-w 100)
                           (/ parent-h 100)))
                 (t      1d0)))))))))

(defparameter *css-length-percentage-scanner*
  (ppcre:create-scanner
   "-?((\\d+(\\.\\d+)?)|(\\.\\d+))([eE]\\d+)?(?:cm|mm|Q|in|pt|pc|px|r?em|r?ex|cap|r?ch|r?ic|r?lh|vw|vi|vh|vb|vmin|vmax|%)?"))

(defun css-parse-all-length-from-string (port str &optional (width-or-height :width) viewport-w viewport-h parent-w parent-h)
  "Parse all CSS <length-percentage> from STR and convert them to pixels."
  (mapcar (lambda (sub)
            (css-parse-length port sub width-or-height viewport-w viewport-h parent-w parent-h))
          (ppcre:all-matches-as-strings *css-length-percentage-scanner* str)))

;; CSS color

(defvar *css-color-keywords* (make-hash-table :test #'equalp :size 149)
  "A map of CSS basic color keywords, from name to LW color spec.

https://www.w3.org/TR/css-color-3/#html4")

(dolist (i '(("aliceblue" "#F0F8FF")       ("antiquewhite" "#FAEBD7")      ("aqua" "#00FFFF")                 ("aquamarine" "#7FFFD4")
             ("azure" "#F0FFFF")           ("beige" "#F5F5DC")             ("bisque" "#FFE4C4")               ("black" "#000000")
             ("blanchedalmond" "#FFEBCD")  ("blue" "#0000FF")              ("blueviolet" "#8A2BE2")           ("brown" "#A52A2A")
             ("burlywood" "#DEB887")       ("cadetblue" "#5F9EA0")         ("chartreuse" "#7FFF00")           ("chocolate" "#D2691E")
             ("coral" "#FF7F50")           ("cornflowerblue" "#6495ED")    ("cornsilk" "#FFF8DC")             ("crimson" "#DC143C")
             ("cyan" "#00FFFF")            ("darkblue" "#00008B")          ("darkcyan" "#008B8B")             ("darkgoldenrod" "#B8860B")
             ("darkgray" "#A9A9A9")        ("darkgreen" "#006400")         ("darkgrey" "#A9A9A9")             ("darkkhaki" "#BDB76B")
             ("darkmagenta" "#8B008B")     ("darkolivegreen" "#556B2F")    ("darkorange" "#FF8C00")           ("darkorchid" "#9932CC")
             ("darkred" "#8B0000")         ("darksalmon" "#E9967A")        ("darkseagreen" "#8FBC8F")         ("darkslateblue" "#483D8B")
             ("darkslategray" "#2F4F4F")   ("darkslategrey" "#2F4F4F")     ("darkturquoise" "#00CED1")        ("darkviolet" "#9400D3")
             ("deeppink" "#FF1493")        ("deepskyblue" "#00BFFF")       ("dimgray" "#696969")              ("dimgrey" "#696969")
             ("dodgerblue" "#1E90FF")      ("firebrick" "#B22222")         ("floralwhite" "#FFFAF0")          ("forestgreen" "#228B22")
             ("fuchsia" "#FF00FF")         ("gainsboro" "#DCDCDC")         ("ghostwhite" "#F8F8FF")           ("gold" "#FFD700")
             ("goldenrod" "#DAA520")       ("gray" "#808080")              ("green" "#008000")                ("greenyellow" "#ADFF2F")
             ("grey" "#808080")            ("honeydew" "#F0FFF0")          ("hotpink" "#FF69B4")              ("indianred" "#CD5C5C")
             ("indigo" "#4B0082")          ("ivory" "#FFFFF0")             ("khaki" "#F0E68C")                ("lavender" "#E6E6FA")
             ("lavenderblush" "#FFF0F5")   ("lawngreen" "#7CFC00")         ("lemonchiffon" "#FFFACD")         ("lightblue" "#ADD8E6")
             ("lightcoral" "#F08080")      ("lightcyan" "#E0FFFF")         ("lightgoldenrodyellow" "#FAFAD2") ("lightgray" "#D3D3D3")
             ("lightgreen" "#90EE90")      ("lightgrey" "#D3D3D3")         ("lightpink" "#FFB6C1")            ("lightsalmon" "#FFA07A")
             ("lightseagreen" "#20B2AA")   ("lightskyblue" "#87CEFA")      ("lightslategray" "#778899")       ("lightslategrey" "#778899")
             ("lightsteelblue" "#B0C4DE")  ("lightyellow" "#FFFFE0")       ("lime" "#00FF00")                 ("limegreen" "#32CD32")
             ("linen" "#FAF0E6")           ("magenta" "#FF00FF")           ("maroon" "#800000")               ("mediumaquamarine" "#66CDAA")
             ("mediumblue" "#0000CD")      ("mediumorchid" "#BA55D3")      ("mediumpurple" "#9370DB")         ("mediumseagreen" "#3CB371")
             ("mediumslateblue" "#7B68EE") ("mediumspringgreen" "#00FA9A") ("mediumturquoise" "#48D1CC")      ("mediumvioletred" "#C71585")
             ("midnightblue" "#191970")    ("mintcream" "#F5FFFA")         ("mistyrose" "#FFE4E1")            ("moccasin" "#FFE4B5")
             ("navajowhite" "#FFDEAD")     ("navy" "#000080")              ("oldlace" "#FDF5E6")              ("olive" "#808000")
             ("olivedrab" "#6B8E23")       ("orange" "#FFA500")            ("orangered" "#FF4500")            ("orchid" "#DA70D6")
             ("palegoldenrod" "#EEE8AA")   ("palegreen" "#98FD98")         ("paleturquoise" "#AFEEEE")        ("palevioletred" "#DB7093")
             ("papayawhip" "#FFEFD5")      ("peachpuff" "#FFDAB9")         ("peru" "#CD853F")                 ("pink" "#FFC0CD")
             ("plum" "#DDA0DD")            ("powderblue" "#B0E0E6")        ("purple" "#800080")               ("red" "#FF0000")
             ("rosybrown" "#BC8F8F")       ("royalblue" "#4169E1")         ("saddlebrown" "#8B4513")          ("salmon" "#FA8072")
             ("sandybrown" "#F4A460")      ("seagreen" "#2E8B57")          ("seashell" "#FFF5EE")             ("sienna" "#A0522D")
             ("silver" "#C0C0C0")          ("skyblue" "#87CEEB")           ("slateblue" "#6A5ACD")            ("slategray" "#708090")
             ("slategrey" "#708090")       ("snow" "#FFFAFA")              ("springgreen" "#00FF7F")          ("steelblue" "#4682B4")
             ("tan" "#D2B48C")             ("teal" "#008080")              ("thistle" "#D8BFD8")              ("tomato" "#FF6347")
             ("turquoise" "#40E0D0")       ("saddlebrown" "#8B4513")       ("violet" "#EE82EE")               ("wheat" "#F5DEB3")
             ("white" "#FFFFFF")           ("whitesmoke" "#F5F5F5")        ("yellow" "#FFFF00")               ("yellowgreen" "#9ACD32")))
  (setf (gethash (first i) *css-color-keywords*)
        (hex-to-spec (second i))))

(setf (gethash "transparent" *css-color-keywords*) (make-rgb 0.0 0.0 0.0 0.0))

;; For scanning valid arguments of the numeric color function
;; e.g. rgb(255, 0, 0)
(defparameter *css-color-numeric-regexp* (ppcre:create-scanner "[\\d\\-\\.%]+"))

(defun css-parse-numeric-color (str)
  "Parse a CSS numerical color value to LW color spec.

https://www.w3.org/TR/css-color-3/#numerical"
  (let ((params (ppcre:all-matches-as-strings *css-color-numeric-regexp* str)))
    (flet ((parse-num (str)
             (let ((num (parse-integer str :junk-allowed t)))
               (if (eql (char str (1- (length str))) #\%)
                 (/ (alexandria:clamp num 0 100) 100)
                 (/ (alexandria:clamp num 0 255) 255)))))
      (let ((func (cond ((string-prefix-p "rgb" str) #'make-rgb)
                        ((string-prefix-p "hsl" str) #'make-hsv))))
        (destructuring-bind (x y z) (mapcar #'parse-num (subseq params 0 3))
          (if (= (length params) 4)
            (let ((alpha (clamp (parse-float (nth 3 params)) 0.0 1.0)))
              (funcall func x y z alpha))
            (funcall func x y z)))))))

(defun css-parse-color (str)
  "Parse a valid CSS color to LW color spec."
  (unless (or (null str) (member str '("none" "auto") :test #'equalp))
    (if (eql (char str 0) #\#)
      (hex-to-spec str)
      (if (or (string-prefix-p "rgb" str) (string-prefix-p "hsl" str))
        (css-parse-numeric-color str)
        (gethash str *css-color-keywords*)))))

;; CSS angle

(defun css-parse-angel (str &optional (start 0))
  "Parse a CSS <angel> to radians from START of the STR.

Return the radians and the end of parsing.

https://www.w3.org/TR/css3-values/#angles"
  (let ((len (length str)))
    (multiple-value-bind (num end-pos) (css-parse-a-number str start)
      (unless (null num)
        (cond ((search "grad" str :start2 end-pos :end2 (min len (+ end-pos 4)))
               (values (deg-to-rad (* num 0.9d0)) (+ end-pos 4)))
              ((search "rad" str :start2 end-pos :end2 (min len (+ end-pos 3)))
               (values num (+ end-pos 3)))
              ((search "turn" str :start2 end-pos :end2 (min len (+ end-pos 4)))
               (values (* num 2pi) (+ end-pos 4)))
              ((search "deg" str :start2 end-pos :end2 (min len (+ end-pos 3)))
               (values (deg-to-rad num) (+ end-pos 3)))
              (t (values (deg-to-rad num) end-pos)))))))

(defun css-parse-all-angels-from-string (str)
  "Parse a CSS <angel> to radians in STR, return as a vector"
  (let ((i 0)
        (len (length str))
        (numbers (make-array 15 :element-type 'double-float :fill-pointer 0 :adjustable t)))
    (declare (type fixnum i len))
    (tagbody
     start
     (if (= i len) (go end) nil)
     (let ((c (char str i)))
       (if (or (digit-char-p c) (member c '(#\+ #\-)))
         (multiple-value-bind (num idx)
             (css-parse-angel str i)
           (if num
             (progn
               (vector-push-extend num numbers)
               (setq i idx)
               (go start))
             (go end)))
         (progn
           (setq i (1+ i))
           (go start))))
     end)
    numbers))

;; CSS transform

(defparameter *transform-scanner*
  (ppcre:create-scanner "(matrix|scale(?:X|Y)?|translate(?:X|Y)?|rotate|skew(?:X|Y)?)\\((?:.|\\s)+?\\)")
  "Scanner for searching CSS <transform-function>s.

https://www.w3.org/TR/css-transforms-1/#transform-functions")

(defun css-parse-transforms (port str &optional viewport-w viewport-h parent-w parent-h)
  "Parse a CSS transform property value to a list of GP:TRANSFORMs.

https://www.w3.org/TR/css-transforms-1/#transform-property"
  (let (transforms)
    (ppcre:do-scans (match-start match-end fname-starts fname-ends
                                 *transform-scanner* str)
      (declare (ignore match-start))
      (setq fname-starts (aref fname-starts 0)
            fname-ends (aref fname-ends 0))
      (let* ((fname (subseq str fname-starts fname-ends))
             (args-str (subseq str (1+ fname-ends) (1- match-end)))
             (args (cond ((member fname '("rotate" "skew" "skewX" "skewY") :test #'string=)
                          (css-parse-all-angels-from-string args-str))
                         ((member fname '("translate" "translateX" "translateY") :test #'string=)
                          (split-sequence '(#\, #\Space) args-str :coalesce-separators t))
                         (t (css-parse-all-numbers-from-string args-str)))))
        (if (string= fname "matrix")
          (push-end (apply #'make-transform (coerce args 'list)) transforms)
          (let ((trans (make-transform)))
            (string-case (fname)
              ("scale" (apply-scale trans (aref args 0) (aref args (if (= (length args) 1) 0 1))))
              ("scaleX" (apply-scale trans (aref args 0) 1))
              ("scaleY" (apply-scale trans 1 (aref args 0)))
              ("translate" (apply-translation
                            trans
                            (css-parse-length port (first args) :width viewport-w viewport-h parent-w parent-h)
                            (css-parse-length port (or (second args) (first args)) :height viewport-w viewport-h parent-w parent-h)))
              ("translateX" (apply-translation
                             trans
                             (css-parse-length port (first args) :width viewport-w viewport-h parent-w parent-h)
                             1))
              ("translateY" (apply-translation
                             trans 1
                             (css-parse-length port (first args) :height viewport-w viewport-h parent-w parent-h)))
              ("rotate" (apply-rotation trans (aref args 0)))
              ("skew"
               (setf (nth 1 trans) (tan (aref args 0)))
               (setf (nth 2 trans) (if (= (length args) 1) 0 (tan (aref args 0)))))
              ("skewX" (setf (nth 1 trans) (tan (aref args 0))))
              ("skewY" (setf (nth 2 trans) (tan (aref args 0)))))
            (push-end trans transforms)))))
    transforms))

;; CSS url

(defparameter *css-url-scanner*
  (ppcre:create-scanner "(?:url\\(\"(.+)\"\\))|(?:url\\('(.+)'\\))|(?:url\\((.+)\\))"))

(defun css-parse-url (str root-node)
  "Return the element targeted by the URL expression inside STR."
  (multiple-value-bind (whole arr)
      (ppcre:scan-to-strings *css-url-scanner* str)
    (declare (ignore whole))
    (let ((url (or (aref arr 0) (aref arr 1) (aref arr 2))))
      (if (or (null url) (not (eql (char url 0) #\#)))
        (error "LW-SVG only support ID url selector.")
        (plump-dom:get-element-by-id root-node (subseq url 1))))))

;; CSS `style` parser

(defun css-parse-style-properties (str)
  "Parse CSS style content STR to a hash-table"
  (setq str (string-trim-whitespace str))
  (let ((table (make-hash-table :test #'equalp)))
    (mapcar (lambda (str)
              (destructuring-bind (name val)
                  (mapcar #'string-trim-whitespace (split-sequence '(#\:) str :coalesce-separators t))
                (setf (gethash name table) val)))
            (mapcar #'string-trim-whitespace (split-sequence '(#\;) str :coalesce-separators t)))
    table))

(defmacro css-parse-class (node)
  `(split-sequence '(#\Space) (plump:attribute ,node "class") :coalesce-separators t))

(defparameter *css-class-name-scanner*
  (ppcre:create-scanner "[A-Za-z][A-Za-z0-9\\-_]*"))

(defparameter *css-id-scanner*
  (ppcre:create-scanner "[A-Za-z][A-Za-z0-9\\-_:\\.]*"))

(defparameter *css-attribute-selector-scanner*
  (ppcre:create-scanner "\\[([A-Za-z]+)((?:~|\\|)?=)?([A-Za-z]+)?\\]"))

(defun css-parse-a-selector (str)
  "Parse one CSS selector from a CSS selector list (separated by #\,)

Return a function that accept one argument PLUMP:NODE, which will
return a non-nil value if the node conforms the selector."
  (let ((index 0)
        (len (length str))
        funcs)
    (tagbody
     start
     (let ((first-char (char str index)))
       (case first-char
         (#\. (multiple-value-bind (start end)
                  (ppcre:scan *css-class-name-scanner* str :start index)
                (let ((cla (subseq str start end)))
                  (push (lambda (node) (member cla (css-parse-class node) :test #'string=)) funcs))
                (setq index end)))
         (#\# (multiple-value-bind (start end)
                  (ppcre:scan *css-id-scanner* str :start index)
                (let ((id (subseq str start end)))
                  (push (lambda (node) (equal id (plump:attribute node "id"))) funcs))
                (setq index end)))
         (#\[ (multiple-value-bind (start end rs re)
                  (ppcre:scan *css-attribute-selector-scanner* str :start index)
                (declare (ignore start))
                (let ((attr (subseq str (aref rs 0) (aref re 0)))
                      (op (when (aref rs 1)
                            (subseq str (aref rs 1) (aref re 1))))
                      (val (when (aref rs 2)
                             (string-trim '(#\") (subseq str (aref rs 2) (aref re 2))))))
                  (push
                   (if op
                     (case (char op 0)
                       (#\= (lambda (node) (equal (plump:attribute node attr) val)))
                       (#\~ (lambda (node)
                              (member val (split-sequence '(#\Space) (plump:attribute node attr))
                                      :test #'equal)))
                       (#\| (lambda (node)
                              (let ((x (plump:attribute node attr)))
                                (or (equal x val)
                                    (and (stringp x)
                                         (string-prefix-p (string-append x "-") val)))))))
                     (lambda (node) (plump:attribute node attr)))
                   funcs))
                (setq index end)))
         (#\Space (let ((prev-func (pop funcs))
                        (sub-func (css-parse-a-selector (subseq str (1+ index)))))
                    (push
                     (lambda (node)
                       (and (loop for parent = (plump:parent node) then (plump:parent parent)
                                  until (or (plump:root-p parent) (null parent))
                                  thereis (funcall prev-func parent))
                            (funcall sub-func node)))
                     funcs)
                    (setq index len)))
         (t (if (alpha-char-p first-char)
              (let* ((end (or (position-if-not #'alpha-char-p str :start index) len))
                     (name (subseq str index end)))
                (push (lambda (node) (equal name (plump:tag-name node))) funcs)
                (setq index end))
              (setq index len)))))
     (if (< index len)
       (go start)))
    (lambda (node)
      (every (lambda (func) (funcall func node)) funcs))))

(defun css-parse-selectors (str)
  "Parse a CSS selector list.

Return a function that accept one argument PLUMP:NODE, which will
return a non-nil value if the node conforms the selector."
  (let ((selectors (mapcar #'css-parse-a-selector
                           (mapcar #'string-trim-whitespace
                                   (split-sequence '(#\,) str :coalesce-separators t)))))
    (lambda (node)
      (some (lambda (func) (funcall func node)) selectors))))

(defparameter *css-style-block-scanner*
  (ppcre:create-scanner "([^\\{\\}]+?)\\{([^\\{\\}]+?)\\}"))

(defun css-parse-style-element (node)
  "Giving a `<style>` element, return an alist which has the function
of the CSS selector as CAR, and a hash-table of corresponding
properties as CDR."
  (let ((str (string-trim-whitespace (plump:text node)))
        result)
    (ppcre:do-scans (ms me rs re *css-style-block-scanner* str)
      (when (every #'identity rs)
        (let ((selector (css-parse-selectors (subseq str (aref rs 0) (aref re 0))))
              (props (css-parse-style-properties (subseq str (aref rs 1) (aref re 1)))))
          (push (cons selector props) result))))
    result))


;; Deal with SVG path data

;; TODO: https://svgwg.org/svg2-draft/implnote.html#ArcCorrectionOutOfRangeRadii
(defun convert-svg-arc (x1 y1 rx ry fai fa fs x2 y2)
  "Conversion from endpoint to center parameterization.

Returns a path element for `GP:DRAW-PATH`

https://svgwg.org/svg2-draft/implnote.html#ArcImplementationNotes"
  (declare (optimize (float 0) (safety 0) (speed 3) (space 0))
           (type double-float x1 y1 rx ry x2 y2 fa fs fai)
           ;(:explain :non-floats)
           )
  (setq fai (* (/ fai 180.0) pi))
  (let* ((sinfai (sin fai))
         (-sinfai (- sinfai))
         (cosfai (cos fai))
         (x1-x2/2 (/ (- x1 x2) 2.0))
         (y1-y2/2 (/ (- y1 y2) 2.0))
         (x1p (+ (* x1-x2/2 cosfai) (* y1-y2/2 sinfai)))
         (y1p (+ (* x1-x2/2 -sinfai) (* y1-y2/2 cosfai)))
         (rx^ (* rx rx))
         (ry^ (* ry ry))
         (lamb (+ (/ (* x1p x1p) rx^) (/ (* y1p y1p) ry^))))
    (declare (type double-float rx ry rx^ ry^))
    (if (> lamb 1)
      ;; We add a very little constant to the enlarged radii,
      ;; to compensate the probable loss during float-point calculation,
      ;; ensure we always have a valid solution for RC
      (setq rx (+ (* rx (sys::sqrt$double-pos lamb)) 1d-6)
            ry (+ (* ry (sys::sqrt$double-pos lamb)) 1d-6)
            rx^ (* rx rx)
            ry^ (* ry ry)))
    (let* ((rc (let ((rc (sys::sqrt$double-pos
                          (- (/ (* rx^ ry^)
                                (+ (* rx^ y1p y1p) (* ry^ x1p x1p)))
                             1.0))))
                 (if (= fa fs) (- rc) rc)))
           (cxp (* rc (/ (* rx y1p) ry)))
           (cyp (* rc (- (/ (* ry x1p) rx))))
           (cx (+ (* cxp cosfai) (* cyp -sinfai) (/ (+ x1 x2) 2.0)))
           (cy (+ (* cxp sinfai) (* cyp cosfai)  (/ (+ y1 y2) 2.0))))
      (flet ((angle (ux uy vx vy)
               (declare (inline angle) (type double-float ux uy vx vy))
               (let ((arc (acos (/ (+ (* ux vx) (* uy vy))
                                   (* (sys::sqrt$double-pos (+ (* ux ux) (* uy uy)))
                                      (sys::sqrt$double-pos (+ (* vx vx) (* vy vy))))))))
                 (declare (type double-float arc))
                 (if (minusp (- (* ux vy) (* uy vx)))
                   (- arc) arc))))
        (let* ((vx (/ (- x1p cxp) rx))
               (vy (/ (- y1p cyp) ry))
               (start (angle 1.0d0 0.0d0 vx vy))
               (sweep (mod (angle vx vy (/ (- (- x1p) cxp) rx) (/ (- (- y1p) cyp) ry))
                           2pi)))
          (declare (type double-float start sweep))
          (if (= fs 0d0)
            (when (plusp sweep) (setq sweep (- sweep 2pi)))
            (when (minusp sweep) (setq sweep (- sweep 2pi))))
          (let ((transform (make-transform)))
            (apply-rotation-around-point transform fai cx cy)
            (list :transform transform (list (list :arc (- cx rx) (- cy ry) (* rx 2.0) (* ry 2.0) (- start) (- sweep))))))))))

(defstruct svg-path-command
  (char #\Null :type character)
  (args (make-array 8 :element-type 'double-float :fill-pointer 0 :adjustable t)
        :type (vector double-float)))

(defmacro do-svg-path-arguments ((args &rest variables) &body body)
  (let ((arg-count (length variables)))
    (with-unique-names (start index len)
      `(let ((,index 0)
             (,len (length ,args)))
         (declare (type fixnum start))
         (tagbody
          ,start
          (let ,(loop for i from 0
                      for var in variables
                      collect (list var `(aref ,args (+ ,index ,i))))
            (declare (type double-float ,@variables))
            ,@body)
          (setq ,index (the fixnum (+ ,index ,arg-count)))
          (when (> ,len ,index)
            (go ,start)))))))

;; https://www.w3.org/TR/2018/CR-SVG2-20181004/paths.html
(defun convert-path-commands (commands)
  "Convert a vector of parsed SVG <path> element data (see
SVG-PARSE-PATH-DATA below) to a vector of PATH that can be passed to
GP:DRAW-PATH."
  (declare (type (vector svg-path-command) commands)
           (optimize (float 0) (safety 0) (speed 3) (debug 0))
           ;(:explain :non-floats :print-original-form)
           )
  (let ((cpx 0d0) (cpy 0d0)
        (ocx2 0d0) (ocy2 0d0)
        (oqx 0d0) (oqy 0d0)
        last-cubic-p
        last-quadratic-p
        last-move-p
        (path (make-array (length commands) :fill-pointer 0 :adjustable t)))
    (declare (type double-float cpx cpy ocx2 ocy2 oqx oqy))
    (macrolet ((push-path (data) `(vector-push-extend ,data path))
               (line-absolute (x y)
                 `(progn
                    (push-path (list :line ,x ,y))
                    (setq cpx ,x cpy ,y)))
               (line-relative (x y)
                 `(progn
                    (setq cpx (+ cpx ,x) cpy (+ cpy ,y))
                    (push-path (list :line cpx cpy)))))
      (dotimes (i (length commands))
        (let* ((command (aref commands i))
               (char (svg-path-command-char command))
               (args (svg-path-command-args command)))
          (case char
            (#\M
             (do-svg-path-arguments (args x y)
               (setq cpx x cpy y)
               (push-path (list (if last-move-p :line :move) x y))
               (setq last-move-p t)))
            (#\m
             (do-svg-path-arguments (args x y)
               (setq cpx (+ cpx x)
                     cpy (+ cpy y))
               (push-path (list (if last-move-p :line :move) cpx cpy))
               (setq last-move-p t)))
            ((or #\Z #\z)
             (push-path (list :close)))
            (#\L
             (do-svg-path-arguments (args x y)
               (line-absolute x y)))
            (#\l
             (do-svg-path-arguments (args x y)
               (line-relative x y)))
            (#\H
             (do-svg-path-arguments (args x)
               (line-absolute x cpy)))
            (#\V
             (do-svg-path-arguments (args y)
               (line-absolute cpx y)))
            (#\h
             (do-svg-path-arguments (args x)
               (line-relative x 0.0d0)))
            (#\v
             (do-svg-path-arguments (args y)
               (line-relative 0.0d0 y)))
            (#\C
             (do-svg-path-arguments (args cx1 cy1 cx2 cy2 nx ny)
               (setq cpx nx cpy ny
                     ocx2 cx2 ocy2 cy2)
               (push-path (list :bezier cx1 cy1 cx2 cy2 nx ny))))
            (#\c
             (do-svg-path-arguments (args rcx1 rcy1 rcx2 rcy2 rnx rny)
               (let ((cx1 (- rcx1 cpx)) (cy1 (- rcy1 cpy))
                     (cx2 (- rcx2 cpx)) (cy2 (- rcy2 cpy))
                     (nx (- rnx cpx)) (ny (- rny cpy)))
                 (setq cpx nx cpy ny
                       ocx2 cx2 ocy2 cy2)
                 (push-path (list :bezier cx1 cy1 cx2 cy2 nx ny)))))
            (#\S
             (do-svg-path-arguments (args cx2 cy2 nx ny)
               (let ((cx1 (if last-cubic-p (- (* cpx 2.0d0) ocx2)
                            cpx))
                     (cy1 (if last-cubic-p (- (* cpy 2.0d0) ocy2)
                            cpy)))
                 (setq cpx nx cpy ny
                       ocx2 cx2 ocy2 cy2
                       last-cubic-p t)
                 (push-path (list :bezier cx1 cy1 cx2 cy2 nx ny)))))
            (#\s
             (do-svg-path-arguments (args rcx2 rcy2 rnx rny)
               (let ((cx1 (if last-cubic-p (- (* cpx 2.0d0) ocx2)
                            cpx))
                     (cy1 (if last-cubic-p (- (* cpy 2.0d0) ocy2)
                            cpy))
                     (cx2 (+ cpx rcx2)) (cy2 (+ cpy rcy2))
                     (nx (+ cpx rnx)) (ny (+ cpy rny)))
                 (setq cpx nx cpy ny
                       ocx2 cx2 ocy2 cy2
                       last-cubic-p t)
                 (push-path (list :bezier cx1 cy1 cx2 cy2 nx ny)))))
            (#\Q
             (do-svg-path-arguments (args qcx1 qcy1 nx ny)
               (let ((cx1 (+ cpx (/ (* (- qcx1 cpx) 2.0d0) 3.0d0)))
                     (cy1 (+ cpy (/ (* (- qcy1 cpy) 2.0d0) 3.0d0)))
                     (cx2 (+ nx (/ (* (- qcx1 nx) 2.0d0) 3.0d0)))
                     (cy2 (+ ny (/ (* (- qcy1 ny) 2.0d0) 3.0d0))))
                 (push-path (list :bezier cx1 cy1 cx2 cy2 nx ny))
                 (setq cpx nx cpy ny
                       oqx qcx1 oqy qcy1
                       last-quadratic-p t))))
            (#\q
             (do-svg-path-arguments (args rqcx1 rqcy1 rnx rny)
               (let ((qcx1 (+ cpx rqcx1))
                     (qcy1 (+ cpy rqcy1))
                     (nx (+ cpx rnx))
                     (ny (+ cpy rny)))
                 (let ((cx1 (+ cpx (/ (* (- qcx1 cpx) 2d0) 3d0)))
                       (cy1 (+ cpy (/ (* (- qcy1 cpy) 2d0) 3d0)))
                       (cx2 (+ nx (/ (* (- qcx1 nx) 2d0) 3d0)))
                       (cy2 (+ ny (/ (* (- qcy1 ny) 2d0) 3d0))))
                   (push-path (list :bezier cx1 cy1 cx2 cy2 nx ny))
                   (setq cpx (+ cpx rnx) cpy (+ cpy rny)
                         oqx qcx1 oqy qcy1
                         last-quadratic-p t)))))
            (#\T
             (do-svg-path-arguments (args nx ny)
               (if last-quadratic-p
                 (let ((qcx1 (- (* cpx 2d0) oqx))
                       (qcy1 (- (* cpy 2d0) oqy)))
                   (let ((cx1 (+ cpx (/ (* (- qcx1 cpx) 2d0) 3d0)))
                         (cy1 (+ cpy (/ (* (- qcy1 cpy) 2d0) 3d0)))
                         (cx2 (+ nx (/ (* (- qcx1 nx) 2d0) 3d0)))
                         (cy2 (+ ny (/ (* (- qcy1 ny) 2d0) 3d0))))
                     (setq oqx qcx1 oqy qcy1 last-quadratic-p t)
                     (push-path (list :bezier cx1 cy1 cx2 cy2 nx ny))))
                 (push-path (list :line nx ny)))
               (setq cpx nx cpy ny)))
            (#\t
             (do-svg-path-arguments (args rnx rny)
               (let ((nx (+ cpx rnx))
                     (ny (+ cpy rny)))
                 (if last-quadratic-p
                   (let ((qcx1 (- (* cpx 2d0) oqx))
                         (qcy1 (- (* cpy 2d0) oqy)))
                     (let ((cx1 (+ cpx (/ (* (- qcx1 cpx) 2d0) 3d0)))
                           (cy1 (+ cpy (/ (* (- qcy1 cpy) 2d0) 3d0)))
                           (cx2 (+ nx (/ (* (- qcx1 nx) 2d0) 3d0)))
                           (cy2 (+ ny (/ (* (- qcy1 ny) 2d0) 3d0))))
                       (setq oqx qcx1 oqy qcy1 last-quadratic-p t)
                       (push-path (list :bezier cx1 cy1 cx2 cy2 nx ny))))
                   (push-path (list :line nx ny)))
                 (setf cpx nx cpy ny))))
            (#\A (do-svg-path-arguments (args rx ry fai fa fs x2 y2)
                   (unless (and (= cpx x2) (= cpy y2))
                     (if (or (= rx 0d0) (= ry 0d0))
                       (line-absolute x2 y2)
                       (progn
                         (if (< rx 0d0) (setq rx (abs rx)))
                         (if (< ry 0d0) (setq ry (abs ry)))
                         (push-path (convert-svg-arc cpx cpy rx ry fai fa fs x2 y2))
                         (setq cpx x2 cpy y2))))))
            (#\a (do-svg-path-arguments (args rx ry fai fa fs x2 y2)
                   (unless (and (= cpx x2) (= cpy y2))
                     (if (or (= rx 0d0) (= ry 0d0))
                       (line-relative x2 y2)
                       (progn
                         (if (< rx 0d0) (setq rx (abs rx)))
                         (if (< ry 0d0) (setq ry (abs ry)))
                         (push-path (convert-svg-arc cpx cpy rx ry fai fa fs (+ cpx x2) (+ cpy y2)))
                         (setq cpx (+ cpx x2) cpy (+ cpy y2))))))))
          (setq last-cubic-p (member char '(#\C #\c #\S #\s))
                last-quadratic-p (member char '(#\Q #\q #\T #\t))
                last-move-p (member char '(#\M #\m)))))
      path)))

(defun svg-parse-path-data (data-string)
  "Parse the svg path `d` property from string to a vector of SVG-PATH-COMMAND for further parsing."
  (declare (type string data-string))
  (let ((commands (make-array 20 :element-type 'vector :fill-pointer 0 :adjustable t))
        current-command
        (i 0)
        (len (length data-string)))
    (declare (type fixnum i len))
    (flet ((start-new-command (command-char)
             (when current-command
               (vector-push-extend current-command commands))
             (setq current-command (make-svg-path-command :char command-char))))
      (tagbody
       start
       (if (= i len) (go end) nil)
       (let ((c (char data-string i)))
         (cond ((alpha-char-p c)
                (start-new-command c)
                (setq i (1+ i))
                (go start))
               ((or (digit-char-p c) (member c '(#\+ #\-)))
                (multiple-value-bind (num idx)
                    (css-parse-a-number data-string i)
                  (if num
                    (progn
                      (vector-push-extend num (svg-path-command-args current-command))
                      (setq i idx)
                      (go start))
                    (go end))))
               (t (setq i (1+ i))
                  (go start))))
       end)
      (start-new-command #\Null)
      commands)))


;; Gradient

(defclass svg-gradient ()
  ((stops :initform nil
          :accessor svg-gradient-stops)))

(defclass svg-linear-gradient (svg-gradient)
  ((x1 :type fixnum :initarg :x1)
   (y1 :type fixnum :initarg :y1)
   (x2 :type fixnum :initarg :x2)
   (y2 :type fixnum :initarg :y2)))

(defclass svg-radial-gradient (svg-gradient)
  ((cx :type fixnum :initarg :cx)
   (cy :type fixnum :initarg :cy)
   (r :type fixnum :initarg :r)
   (fx :type fixnum :initarg :fx)
   (fy :type fixnum :initarg :fy)
   (fr :type fixnum :initarg :fr)
   (mx :type fixnum :initarg :mx)
   (my :type fixnum :initarg :my)))

(defstruct svg-gradient-stop
  (offset 0d0 :type double-float)
  color
  opacity)

(defun svg-parse-gradient (node left top right bottom)
  (string-case  ((plump-dom:tag-name node))
    ("linearGradient" (svg-parse-linear-gradient node left top right bottom))
    ("radialGradient" (svg-parse-radial-gradient node left top right bottom))))

(defun svg-parse-linear-gradient (node left top right bottom)
  (declare (optimize (safety 0) (fixnum-safety 0)))
  (flet ((parse-vector-value (node key start end)
           (when-let (str (gethash key (plump:attributes node)))
             (let ((len (length str)))
               (if (eql #\% (char str (1- len)))
                 (+ start
                    (round (* (- end start)
                              (/ (parse-float str :end (1- len)) 100d0))))
                 (parse-integer str)))))
         (parse-stop-value (node key)
           (when-let (str (gethash key (plump:attributes node)))
             (let ((len (length str)))
               (if (eql #\% (char str (1- len)))
                 (/ (parse-float str :end (1- len)) 100d0)
                 (parse-integer str))))))
    (let ((grad (make-instance 'svg-linear-gradient
                               :x1 (or (parse-vector-value node "x1" left right) left)
                               :y1 (or (parse-vector-value node "y1" top bottom) top)
                               :x2 (or (parse-vector-value node "x2" left right) right)
                               :y2 (or (parse-vector-value node "y2" top bottom) top)))
          (stops (plump-dom:get-elements-by-tag-name node "stop")))
      (dolist (stop stops)
        (push (make-svg-gradient-stop
               :offset (or (parse-stop-value stop "offset") 0d0)
               :color (or (css-parse-color (gethash "stop-color" (plump:attributes stop))) :black)
               :opacity (or (parse-stop-value stop "stop-opacity") 1d0))
              (svg-gradient-stops grad)))
      (setf (svg-gradient-stops grad)
            (sort (svg-gradient-stops grad)
                  (lambda (grad1 grad2)
                    (< (svg-gradient-stop-offset grad1) (svg-gradient-stop-offset grad2)))))
      (let ((start (copy-svg-gradient-stop
                    (first (svg-gradient-stops grad))))
            (end (copy-svg-gradient-stop
                  (car (last (svg-gradient-stops grad))))))
        (when (/= (svg-gradient-stop-offset start) 0d0)
          (setf (svg-gradient-stop-offset start) 0d0)
          (push start (svg-gradient-stops grad)))
        (when (/= (svg-gradient-stop-offset end) 1d0)
          (setf (svg-gradient-stop-offset end) 1d0)
          (push-end end (svg-gradient-stops grad))))
      grad)))

(defun svg-parse-radial-gradient (node left top right bottom)
  (declare (optimize (safety 0) (fixnum-safety 0)))
  (flet ((parse-vector-value (node key start end)
           (when-let (str (gethash key (plump:attributes node)))
             (let ((len (length str)))
               (if (eql #\% (char str (1- len)))
                 (+ start
                    (round (* (- end start)
                              (/ (parse-float str :end (1- len)) 100d0))))
                 (parse-integer str)))))
         (parse-stop-value (node key)
           (when-let (str (gethash key (plump:attributes node)))
             (let ((len (length str)))
               (if (eql #\% (char str (1- len)))
                 (/ (parse-float str :end (1- len)) 100d0)
                 (parse-integer str))))))
    (let* ((cx (or (parse-vector-value node "cx" left right) left))
           (cy (or (parse-vector-value node "cy" top bottom) top))
           (r (or (parse-vector-value node "r" 0 (- right left)) (round (- right left) 2)))
           (fx (or (parse-vector-value node "fx" left right) cx))
           (fy (or (parse-vector-value node "fy" top bottom) cy))
           (fr (or (parse-vector-value node "fr" left right) 0))
           ;; 设大圆为Cr，圆心为R；小圆为Cf，圆心为F。在Cr上任取一点M=(x1, y1)，Cf上任取一点N=(x2, y2)，
           ;; 使MR平行于NF。同理取得点P, Q。求直线MN与直线PQ的交点为(mx, my)
           ;; https://www.cnblogs.com/lingge1992/p/6738487.html
           (x1 cx)
           (y1 (+ cy r))
           (x2 fx)
           (y2 (+ fy fr))
           (x3 (+ cx r))
           (y3 cy)
           (x4 (+ fx fr))
           (y4 fy)
           (a1 (- y2 y1))
           (b1 (- x1 x2))
           (c1 (- (* x2 y1) (* y2 x1)))
           (a2 (- y4 y3))
           (b2 (- x3 x4))
           (c2 (- (* x4 y3) (* y4 x3)))
           (d (- (* a1 b2) (* a2 b1)))
           (mx (if (zerop d) cx (round (/ (- (* b1 c2) (* b2 c1)) d))))
           (my (if (zerop d) cy (round (/ (- (* c1 a2) (* c2 a1)) d))))
           (grad (make-instance 'svg-radial-gradient
                                :cx cx :cy cy :r r :fx fx :fy fy :fr fr :mx mx :my my))
           (stops (plump-dom:get-elements-by-tag-name node "stop")))
      (declare (type fixnum cx cy r fx fy fr))
      (dolist (stop stops)
        (push (make-svg-gradient-stop
               :offset (or (parse-stop-value stop "offset") 0d0)
               :color (or (css-parse-color (gethash "stop-color" (plump:attributes stop))) :black)
               :opacity (or (parse-stop-value stop "stop-opacity") 1d0))
              (svg-gradient-stops grad)))
      (setf (svg-gradient-stops grad)
            (sort (svg-gradient-stops grad)
                  (lambda (grad1 grad2)
                    (< (svg-gradient-stop-offset grad1) (svg-gradient-stop-offset grad2)))))
      (let ((start (copy-svg-gradient-stop
                    (first (svg-gradient-stops grad))))
            (end (copy-svg-gradient-stop
                  (car (last (svg-gradient-stops grad))))))
        (when (/= (svg-gradient-stop-offset start) 0d0)
          (setf (svg-gradient-stop-offset start) 0d0)
          (push start (svg-gradient-stops grad)))
        (when (/= (svg-gradient-stop-offset end) 1d0)
          (setf (svg-gradient-stop-offset end) 1d0)
          (push-end end (svg-gradient-stops grad))))
      grad)))

(defgeneric svg-gradient-color (x y grad)
  (:method (x y (grad svg-linear-gradient))
   (declare (optimize (safety 0) (float 0) (fixnum-safety 0))
            (type fixnum x y))
   (let* ((x1 (slot-value grad 'x1))
          (y1 (slot-value grad 'y1))
          (x2 (slot-value grad 'x2))
          (y2 (slot-value grad 'y2))
          (v1x (- x1 x))
          (v1y (- y1 y))
          (v2x (- x1 x2))
          (v2y (- y1 y2))
          (v2len (sys::sqrt$pos (+ (* v2x v2x) (* v2y v2y))))
          (percentage (/ (+ (* v1x v2x) (* v1y v2y)) v2len v2len))
          (stops (svg-gradient-stops grad))
          before after)
     (declare (type fixnum x1 y1 x2 y2))
     (loop for prev-stop = (first stops) then stop
           for stop in (cdr stops)
           until (<= (svg-gradient-stop-offset prev-stop)
                     percentage
                     (svg-gradient-stop-offset stop))
           finally (setq before prev-stop
                         after stop))
     (let ((ratio (/ (- percentage (svg-gradient-stop-offset before))
                     (- (svg-gradient-stop-offset after) (svg-gradient-stop-offset before)))))
       (flet ((lerp-color (func)
                (lerp ratio
                      (funcall func (svg-gradient-stop-color before))
                      (funcall func (svg-gradient-stop-color after)))))
         (make-rgb
          (lerp-color #'color-red) (lerp-color #'color-green) (lerp-color #'color-blue)
          (lerp ratio (svg-gradient-stop-opacity before) (svg-gradient-stop-opacity after)))))))
  
  (:method (x y (grad svg-radial-gradient))
   (declare (optimize (safety 0) (float 0) (fixnum-safety 0))
            (type fixnum x y))
   (let* ((cx (slot-value grad 'cx))
          (cy (slot-value grad 'cy))
          (r (slot-value grad 'r))
          (fx (slot-value grad 'fx))
          (fy (slot-value grad 'fy))
          (fr (slot-value grad 'fr))
          (mx (slot-value grad 'mx))
          (my (slot-value grad 'my))
          (stops (svg-gradient-stops grad)))
     (declare (type fixnum cx cy r fx fy fr mx my))
     ;; 设大圆为Cr，圆心为R；小圆为Cf，圆心为F；已知点M(mx, my)，N(x, y)， 射线MN交Cf于点P, 交Cr于点Q, 求PN:PQ
     ;; 首先解三角形MFP得到MP，根据余弦定理，PF²=MF²+MP²-2MF⋅MP⋅cos∠PMF；
     ;; 使用向量点积法求得cos∠PMF，代入等式并解二元一次方程得到线段MP的长
     ;; 根据定义知PF平行于QR, 可知三角形PMF与三角形QMR相似，因而可得MF:MR=MP:MQ，进而求得线段MQ的长
     ;; 根据勾股定理求得MN，PN:PQ=(MN-MP):(MQ-MP)，计算完毕
     (cond ((<= (sys::sqrt$pos (+ (* (- x fx) (- x fx)) (* (- y fy) (- y fy)))) fr)
            (svg-gradient-stop-color (first stops)))
           ((>= (sys::sqrt$pos (+ (* (- x cx) (- x cx)) (* (- y cy) (- y cy)))) r)
            (svg-gradient-stop-color (car (last stops))))
           (t
            (let* ((vMFx (- fx mx))
                   (vMFy (- fy my))
                   (lenMF (float (sys::sqrt$pos (+ (* vMFx vMFx) (* vMFy vMFy))) 0d0))
                   (vMNx (- x mx))
                   (vMNy (- y my))
                   (lenMN (float (sys::sqrt$pos (+ (* vMNx vMNx) (* vMNy vMNy))) 0d0))
                   (MFMN (* lenMF lenMN))
                   (cosR (if (zerop MFMN) 0d0
                           (/ (+ (* vMNx vMFx) (* vMNy vMFy))
                              MFMN)))
                   (lenMP (if (zerop MFMN) 0d0
                                 (/ (+ (* 2d0 lenMF cosR) (sys::sqrt$pos (+ (* 4d0 (* fr fr)) (* cosR cosR))))
                                    2d0)))
                   (lenMQ (if (zerop MFMN) (float r 0d0)
                                 (* (/ lenMP fr) r)))
                   (percentage (clamp (/ (- lenMN lenMP) (- lenMQ lenMP)) 0d0 1d0))
                   before after)
              (declare (type fixnum vMFx vMFy vMNx vMNy)
                       (type double-float cosR lenMP lenMQ))
              (loop for prev-stop = (first stops) then stop
                    for stop in (cdr stops)
                    until (<= (svg-gradient-stop-offset prev-stop)
                              percentage
                              (svg-gradient-stop-offset stop))
                    finally (setq before prev-stop
                                  after stop))
              (let ((ratio (/ (- percentage (svg-gradient-stop-offset before))
                              (- (svg-gradient-stop-offset after) (svg-gradient-stop-offset before)))))
                (flet ((lerp-color (func)
                         (lerp ratio
                               (funcall func (svg-gradient-stop-color before))
                               (funcall func (svg-gradient-stop-color after)))))
                  (make-rgb
                   (lerp-color #'color-red) (lerp-color #'color-green) (lerp-color #'color-blue)
                   (lerp ratio (svg-gradient-stop-opacity before) (svg-gradient-stop-opacity after)))))))))))


;; SVG render

(defvar *svg-presentation-attributes*
  '("x"                            "y"                          "cx"                          "cy"
    "r"                            "rx"                         "ry"                          "width"
    "height"                       "d"                          "fill"                        "transform"
    "alignment-baseline"           "baseline-shift"             "clip-path"                   "clip-rule"
    "color"                        "color-interpolation"        "color-interpolation-filters" "color-rendering"
    "cursor"                       "direction"                  "display"                     "dominant-baseline"
    "fill-opacity"                 "fill-rule"                  "filter"                      "flood-color"
    "flood-opacity"                "font-family"                "font-size"                   "font-size-adjust"
    "font-stretch"                 "font-style"                 "font-variant"                "font-weight"
    "glyph-orientation-horizontal" "glyph-orientation-vertical" "image-rendering"             "letter-spacing"
    "lighting-color"               "marker-end"                 "marker-mid"                  "marker-start"
    "mask"                         "opacity"                    "overflow"                    "paint-order"
    "pointer-events"               "shape-rendering"            "stop-color"                  "stop-opacity"
    "stroke"                       "stroke-dasharray"           "stroke-dashoffset"           "stroke-linecap"
    "stroke-linejoin"              "stroke-miterlimit"          "stroke-opacity"              "stroke-width"
    "text-anchor"                  "text-decoration"            "text-overflow"               "text-rendering"
    "unicode-bidi"                 "vector-effect"              "visibility"                  "white-space"
    "word-spacing"                 "writing-mode"               ))

(defun create-renderer (port node &optional (root-node node) (container-attributes (make-hash-table :test #'equalp)))
  "Compile a SVG DOM element into a \"renderer\" function, which can
draw the SVG to the specified PORT.

PORT: a graphics-port that is used to provide necessary display
informations to compile the SVG.

Note that the returned function can be used to draw the SVG at any
graphics port, but the graphics informations, i.e. fonts, relative
size, screen DPI, will be embedded using the attributes from this
PORT.

ROOT-NODE: The root node of the SVG DOM, in PLUMP:NODE, which can
be NODE or its parent.  It is used to search for DOM elements being
referenced by SVG, to build re-used graphic objects.

NODE: the PLUMP:NODE of the SVG element being compiled.

CONTAINER-ATTRIBUTES: A hash-table of inherit attributes that are used
in recursive parsing. Not needed when parsing a standalone SVG
element."
  (declare (optimize (float 0) (safety 0) (speed 3))
           (type hash-table container-attributes)
           ;(:explain :types)
           )
  (let ((tag (plump-dom:tag-name node))
        (new-attrs (plump-dom:attributes node)))
    (declare (type string tag))
    ;; Processing CSS style properties and merge them into NEW-ATTRS
    (let ((styles (ensure-gethash
                   "css-styles" container-attributes
                   (mapcan #'css-parse-style-element
                           (plump-dom:get-elements-by-tag-name root-node "style")))))
      ;; There're some problems here, so it only conforms SVG 1.1 but not 2
      (loop for (pred . attrs) in styles
            when (funcall pred node)
              do (nmerge-tables new-attrs attrs))
      (when-let (inline-style (plump-dom:attribute node "style"))
        (nmerge-tables new-attrs (css-parse-style-properties inline-style))))
    (labels (;; Helper functions & Getter of presentation attributes
             (svg-parse-length (val width-or-height)
               (if (stringp val)
                 (rectangle-bind (x y w h)
                     (gethash "viewBox" container-attributes)
                   (declare (ignore x y))
                   (css-parse-length
                    port val width-or-height w h
                    (gethash "width" container-attributes)
                    (gethash "height" container-attributes)))
                 val))
             (svg-parse-all-length (val width-or-height)
               (if (stringp val)
                 (rectangle-bind (x y w h)
                     (gethash "viewBox" container-attributes)
                   (declare (ignore x y))
                   (css-parse-all-length-from-string
                    port val width-or-height w h
                    (gethash "width" container-attributes)
                    (gethash "height" container-attributes)))
                 val))
             (get-attr (key &optional default)
               (declare (inline get-attr))
               (gethash key new-attrs
                        (gethash key container-attributes
                                 default)))
             (get-fill ()
               (declare (inline get-fill))
               (when-let (val (get-attr "fill"))
                 (string-case (val)
                   ("context-fill"
                    (setq val (gethash "fill" container-attributes)))
                   ("context-stroke"
                    (setq val (gethash "stroke" container-attributes)))
                   (t nil))
                 (cond ((member val '(nil "auto") :test #'equalp) :black)
                       ((string-equal val "none") nil)
                       ((search "url" val) (css-parse-url val root-node))
                       (t (css-parse-color val)))))
             (get-fill-rule ()
               (declare (inline get-fill-rule))
               (if-let (val (get-attr "fill-rule"))
                   (if (stringp val)
                     (if (string-equal val "evenodd") :even-odd :winding)
                     val)
                 :winding))
             (get-stroke ()
               (declare (inline get-stroke))
               (when-let (val (get-attr "stroke"))
                 (string-case (val)
                   ("context-fill"
                    (setq val (gethash "fill" container-attributes)))
                   ("context-stroke"
                    (setq val (gethash "stroke" container-attributes)))
                   (t nil))
                 (cond ((member val '(nil "auto" "none") :test #'equalp) nil)
                       ((search "url" val) (css-parse-url val root-node))
                       (t (css-parse-color val)))))
             (get-stroke-dasharray ()
               (declare (inline get-stroke-dasharray))
               (when-let (val (get-attr "stroke-dasharray"))
                 (if (stringp val)
                   (mapcar #'round (svg-parse-all-length val :width))
                   val)))
             (get-linecap ()
               (declare (inline get-linecap))
               (string-case ((get-attr "stroke-linecap"))
                 ("round" :round)
                 ("square" :square)
                 (t :butt)))
             (get-linejoin ()
               (declare (inline get-linejoin))
               (string-case ((get-attr "stroke-linejoin"))
                 ("round" :round)
                 ("bevel" :bevel)
                 (t :miter)))
             (get-a-length (name)
               (declare (inline get-a-length))
               (svg-parse-length
                (get-attr name)
                (if (or (find #\y name) (search "height" name)) :height :width)))
             ;; Running draw-path
             (run-draw-path (trans-origin-x trans-origin-y path)
               (declare (type double-float trans-origin-x trans-origin-y))
               (let* ((transform (make-transform))
                      (svg-transform (gethash "svg-transform" container-attributes))
                      (container-transforms (gethash "container-transforms" container-attributes))
                      (self-transforms (gethash "transform" new-attrs))
                      (fill (get-fill))
                      (stroke (get-stroke))
                      (stroke-width (get-attr "stroke-width" 1))
                      (linecap (get-linecap))
                      (linejoin (get-linejoin))
                      (dash (get-stroke-dasharray))
                      (fill-rule (get-fill-rule))
                      (args (list path 0 0 :fill-rule fill-rule
                                  :line-end-style linecap :line-join-style linejoin
                                  :dashed (if dash t nil) :dash dash)))
                 ;; These transforms has not been parsed, so parse them first
                 (rectangle-bind (x y w h)
                     (gethash "viewBox" container-attributes)
                   (declare (ignore x y))
                   (when self-transforms
                     (setq self-transforms
                           (css-parse-transforms
                            port self-transforms w h
                            (gethash "width" container-attributes)
                            (gethash "height" container-attributes))))
                   (when container-transforms
                     (setq container-transforms
                           (css-parse-transforms
                            port container-transforms w h
                            (gethash "width" container-attributes)
                            (gethash "height" container-attributes)))))
                 (apply-translation transform (- trans-origin-x) (- trans-origin-y))
                 (dolist (trans (append self-transforms container-transforms))
                   (premultiply-transforms transform trans))
                 (apply-translation transform trans-origin-x trans-origin-y)
                 ;; Apply svg-transform in the end. It's relative with (0, 0);
                 (when svg-transform (postmultiply-transforms transform svg-transform))
                 (setq args (nconc args (list :transform transform)))
                 (when (plump-dom:node-p stroke)
                   (setq stroke :black))
                 ;; Opacity
                 (flet ((parse-opacity (val)
                          (if (stringp val) (clamp (parse-float val) 0.0 1.0) (if val 1.0 nil)))
                        (with-alpha (color alpha)
                          (when color
                            (setq color (ensure-rgb (color:get-color-spec color)))
                            (make-rgb (color-red color)
                                      (color-green color)
                                      (color-blue color)
                                      (* (color-alpha color) alpha)))))
                   (when-let (op (or (parse-opacity (gethash "fill-opacity" container-attributes))
                                     (parse-opacity (gethash "opacity" container-attributes))))
                     (setq fill (with-alpha fill op)))
                   (when-let (op (or (parse-opacity (gethash "fill-opacity" new-attrs))
                                     (parse-opacity (gethash "opacity" new-attrs))))
                     (setq fill (with-alpha fill op)))
                   (when-let (op (or (parse-opacity (gethash "stroke-opacity" container-attributes))
                                     (parse-opacity (gethash "opacity" container-attributes))))
                     (setq stroke (with-alpha stroke op)))
                   (when-let (op (or (parse-opacity (gethash "stroke-opacity" new-attrs))
                                     (parse-opacity (gethash "opacity" new-attrs))))
                     (setq stroke (with-alpha stroke op))))
                 ;; Deal with gradients
                 (if (plump-dom:node-p fill)
                   (let ((grad-trans (gethash "gradientTransform" (plump-dom:attributes fill)))
                         (grad-units (gethash "gradientUnits" (plump-dom:attributes fill)))
                         left top right bottom)
                     ;; Parse gradient transforms
                     (rectangle-bind (x y w h)
                         (gethash "viewBox" container-attributes)
                       (declare (ignore x y))
                       (if grad-trans
                         (setq grad-trans
                               (css-parse-transforms port grad-trans w h
                                                     (gethash "width" container-attributes)
                                                     (gethash "height" container-attributes)))
                         (setq grad-trans (make-transform)))
                       ; Parse gradient units and find corresponding drawing box
                       (if (equal grad-units "userSpaceOnUse")
                         (setq left 0 top 0 right w bottom h)
                         ;; Do a virtrual drawing to collect the bound of the path
                         (capi:with-geometry port
                           (gp:with-pixmap-graphics-port (pixmap port capi:%width% capi:%height% :relative t :collect t)
                             (gp:draw-path pixmap path 0 0)
                             (multiple-value-setq (left top right bottom) (gp:get-bounds pixmap))))))
                     (let ((grad (svg-parse-gradient fill left top right bottom)))
                       (lambda (port)
                         (gp:with-graphics-mask (port (list :path path :fill-rule fill-rule) :mask-transform transform)
                           (do ((x left (1+ x)))
                               ((> x right))
                             (do ((y top (1+ y)))
                                 ((> y bottom))
                               (gp:draw-point port x y :foreground (svg-gradient-color x y grad) :transform grad-trans))))
                         (when stroke
                           (apply #'gp:draw-path port (append args (list :foreground stroke :thickness stroke-width)))))))
                   ;; No gradient, yield normal drawing function
                   (lambda (port)
                     (when fill
                       (apply #'gp:draw-path port (append args (list :foreground fill :filled t))))
                     (if stroke
                       (apply #'gp:draw-path port (append args (list :foreground stroke :thickness stroke-width)))
                       (if (not fill) ; Draw the path in default color if nether `fill` nor `stroke` specified.
                         (apply #'gp:draw-path port (append args (list :thickness stroke-width))))))))))
      (string-case (tag)
        ("path" (run-draw-path
                 0d0 0d0
                 (convert-path-commands (svg-parse-path-data (gethash "d" new-attrs)))))
        ("rect" (let ((x (get-a-length "x"))
                      (y (get-a-length "y"))
                      (w (get-a-length "width"))
                      (h (get-a-length "height"))
                      (rx (get-a-length "rx"))
                      (ry (get-a-length "ry")))
                  (declare (type double-float x y w h))
                  (cond ((and rx (null ry)) (setq ry (min (/ h 2d0) (* (/ rx w) h))))
                        ((and (null rx) ry) (setq rx (min (/ w 2d0) (* (/ ry h) w)))))
                  (run-draw-path
                   x y
                   (if (or (null rx) (and (= rx 0d0) (= ry 0d0)))
                     `((:move ,x ,y) (:line ,(+ x w) ,y) (:line ,(+ x w) ,(+ y h)) (:line ,x ,(+ y h)) (:close))
                     (let ((2rx (* rx 2d0))
                           (2ry (* ry 2d0))
                           (-pi-by-2 (- pi-by-2)))
                       `((:move ,(+ x rx) ,y) (:line ,(- (+ x w) rx) ,y) (:arc ,(- (+ x w) 2rx) ,y ,2rx ,2ry ,pi-by-2 ,-pi-by-2)
                         (:line ,(+ x w) ,(- (+ y h) ry)) (:arc ,(- (+ x w) 2rx) ,(- (+ y h) 2ry) ,2rx ,2ry 0d0 ,-pi-by-2)
                         (:line ,(+ x rx) ,(+ y h)) (:arc ,x ,(- (+ y h) 2ry) ,2rx ,2ry ,-pi-by-2 ,-pi-by-2)
                         (:line ,x ,(+ y ry)) (:arc ,x ,y ,2rx ,2ry ,(- pi) ,-pi-by-2)
                         (:close)))))))
        ("circle" (let ((cx (get-a-length "cx"))
                        (cy (get-a-length "cy"))
                        (r (get-a-length "r")))
                    (declare (type double-float cx cy r))
                    (run-draw-path cx cy (list (list :arc (- cx r) (- cy r) (* r 2d0) (* r 2d0) 0d0 2pi t)))))
        ("ellipse" (let ((cx (get-a-length "cx")) (cy (get-a-length "cy"))
                         (rx (get-a-length "rx")) (ry (get-a-length "ry")))
                     (declare (type double-float cx cy rx ry))
                     (run-draw-path cx cy (list (list :arc (- cx rx) (- cy ry) (* rx 2d0) (* ry 2d0) 0d0 2pi t)))))
        ("line" (let ((x1 (get-a-length "x1")) (y1 (get-a-length "y1"))
                      (x2 (get-a-length "x2")) (y2 (get-a-length "y2")))
                  (declare (type double-float x1 y1 x2 y2))
                  (run-draw-path (/ (+ x1 x2) 2d0) (/ (+ y1 y2) 2d0)
                                 (list (list :move x1 y1) (list :line x2 y2)))))
        ("polyline" (let* ((points (css-parse-all-numbers-from-string (get-attr "points")))
                           (path (make-array (1+ (/ (length points) 2)) :element-type 'list :fill-pointer 0))
                           (start-x (aref points 0)) (start-y (aref points 1))
                           mid-x mid-y)
                      (vector-push (list :move start-x start-y) path)
                      (loop for i fixnum from 2 to (1- (length points)) by 2
                            for x double-float = (aref points i)
                            and y double-float = (aref points (1+ i))
                            do (vector-push (list :line x y) path)
                            minimize x into min-x
                            maximize x into max-x
                            minimize y into min-y
                            maximize y into max-y
                            finally (setq min-x (min min-x start-x)
                                          max-x (max max-x start-x)
                                          min-y (min min-y start-y)
                                          max-y (max max-y start-y)
                                          mid-x (/ (+ min-x max-x) 2d0)
                                          mid-y (/ (+ min-y max-y) 2d0)))
                      (run-draw-path mid-x mid-y path)))
        ("polygon" (let* ((points (css-parse-all-numbers-from-string (get-attr "points")))
                          (path (make-array (+ (/ (length points) 2) 2) :element-type 'list :fill-pointer 0))
                          (start-x (aref points 0))
                          (start-y (aref points 1))
                          mid-x mid-y)
                     (vector-push (list :move start-x start-y) path)
                     (loop for i fixnum from 2 to (1- (length points)) by 2
                           for x double-float = (aref points i)
                           and y double-float = (aref points (1+ i))
                           do (vector-push (list :line x y) path)
                           minimize x into min-x
                           maximize x into max-x
                           minimize y into min-y
                           maximize y into max-y
                           finally (setq min-x (min min-x start-x)
                                         max-x (max max-x start-x)
                                         min-y (min min-y start-y)
                                         max-y (max max-y start-y)
                                         mid-x (/ (the double-float (+ min-x max-x)) 2d0)
                                         mid-y (/ (the double-float (+ min-y max-y)) 2d0)))
                     (vector-push (list :close) path)
                     (run-draw-path mid-x mid-y path)))
        (t (when (member tag '("a" "clipPath" "defs" "g" "marker" "mask" "pattern" "svg" "switch" "symbol" "unknown" "use")
                         :test #'string=)
             (let ((new-table (copy-hash-table container-attributes)))
               (maphash (lambda (k v) (setf (gethash k new-table) v))
                        new-attrs)
               ;; If a transform is specified for a container,
               ;; since we know nether the transform-origin, nor the parent's size it relative with,
               ;; we need to store it for later parse & apply. It should be a STRING.
               (when-let (new-transform (gethash "transform" new-attrs))
                 ;; When transformed containers nested, append them after new transforms.
                 (when-let (trans (gethash "container-transforms" new-table))
                   (setq new-transform (string-append new-transform " " trans)))
                 (setf (gethash "container-transforms" new-table) new-transform))
               (string-case (tag)
                 ("svg"
                  (capi:with-geometry port
                    (let ((viewbox (if-let (val (gethash "viewBox" new-attrs))
                                       (coerce (css-parse-all-numbers-from-string val) 'list)
                                     nil)))
                      (rectangle-bind (viewbox-l viewbox-t viewbox-w viewbox-h)
                          viewbox
                        (declare (type double-float viewbox-l viewbox-t viewbox-w viewbox-h))
                        (let* ((new-x (if-let (val (gethash "x" new-attrs))
                                          (svg-parse-attribute-by-name "x" val port container-attributes root-node)
                                        0d0))
                               (new-y (if-let (val (gethash "y" new-attrs))
                                          (svg-parse-attribute-by-name "y" val port container-attributes root-node)
                                        0d0))
                               (new-w (svg-parse-attribute-by-name "width" (gethash "width" new-attrs) port container-attributes root-node))
                               (new-h (svg-parse-attribute-by-name "height" (gethash "height" new-attrs) port container-attributes root-node))
                               (transform (make-transform))
                               (preserve-aspect (if-let (val (gethash "preserveAspectRatio" new-attrs))
                                                    (split-sequence '(#\Space) val)
                                                  '("xMidYMid" "meet")))
                               (align (first preserve-aspect))
                               (scale (or (second preserve-aspect) "meet")))
                          (declare (type double-float new-x new-y)
                                   (type string align scale))
                          (cond ((and (null new-w) new-h) (setq new-w new-h))
                                ((and new-w (null new-h)) (setq new-h new-w))
                                ((null new-h) (setq new-w viewbox-w new-h viewbox-h)))
                          (locally (declare (type double-float new-w new-h))
                            (if (string= align "none") ; Do not preserve aspect
                              (progn
                                ;; Scale viewBox to fit SVG `width` and `height`
                                (when (and viewbox (or new-w new-h))
                                  (apply-scale transform
                                               (/ new-w viewbox-w)
                                               (/ new-h viewbox-h)))
                                ;; Move the left-top of the viewBox to (x, y)
                                (apply-translation transform new-x new-y))
                              (let* ((xalign (subseq align 0 4))
                                     (yalign (subseq align 4 8))
                                     (svg-align-x 0d0)
                                     (svg-align-y 0d0)
                                     (viewbox-align-x 0d0)
                                     (viewbox-align-y 0d0))
                                (declare (type double-float svg-align-x svg-align-y viewbox-align-x viewbox-align-y)
                                         (type string xalign yalign))
                                (string-case (xalign)
                                  ("xMin" (setq svg-align-x new-x
                                                viewbox-align-x viewbox-l))
                                  ("xMid" (setq svg-align-x (+ new-x (/ new-w 2d0))
                                                viewbox-align-x (+ viewbox-l (/ viewbox-w 2d0))))
                                  ("xMax" (setq svg-align-x (+ new-x new-w)
                                                viewbox-align-x (+ viewbox-l viewbox-w))))
                                (string-case (yalign)
                                  ("YMin" (setq svg-align-y new-y
                                                viewbox-align-y viewbox-t))
                                  ("YMid" (setq svg-align-y (+ new-y (/ new-h 2d0))
                                                viewbox-align-y (+ viewbox-t (/ viewbox-h 2d0))))
                                  ("YMax" (setq svg-align-y (+ new-y new-h)
                                                viewbox-align-y (+ viewbox-t viewbox-h))))
                                ;; Apply scaling, and align selected points
                                (apply-translation transform (- viewbox-align-x) (- viewbox-align-y))
                                (let* ((x-ratio (/ new-w viewbox-w))
                                       (y-ratio (/ new-h viewbox-h))
                                       (ratio (if (string= scale "meet")
                                                (min x-ratio y-ratio)
                                                (max x-ratio y-ratio))))
                                  (apply-scale transform ratio ratio))
                                (apply-translation transform svg-align-x svg-align-y))))
                          ;; If the element itself is `svg`, we assume the transform-origin is 0, 0;
                          ;; no additional translation needed, so we just multiply them into the svg-transform
                          (when-let (container-transforms (gethash "container-transforms" new-table))
                            (dolist (trans container-transforms)
                              (postmultiply-transforms transform trans))
                            (remhash "container-transforms" new-table))
                          (when-let (prev-transform (gethash "svg-transform" container-attributes))
                            (postmultiply-transforms transform prev-transform))
                          ;; Store modified attributes
                          (setf (gethash "svg-transform" new-table) transform
                                (gethash "viewBox" new-table) viewbox
                                (gethash "width" new-table) new-w
                                (gethash "height" new-table) new-h)
                          ;; Pack a drawing function
                          (let ((funcs (delete nil (loop for child across (plump-dom:children node)
                                                         when (plump-dom:element-p child)
                                                           collect (create-renderer port child root-node new-table)))))
                            (lambda (port)
                              (dolist (func funcs)
                                (funcall func port)))))))))
                 ("use"
                  (let* ((id (string-left-trim '(#\#) (or (gethash "href" new-attrs)
                                                          (gethash "xlink:href" new-attrs))))
                         (new-x (if-let (val (gethash "x" new-attrs))
                                    (svg-parse-attribute-by-name "x" val port container-attributes root-node)
                                  0d0))
                         (new-y (if-let (val (gethash "y" new-attrs))
                                    (svg-parse-attribute-by-name "y" val port container-attributes root-node)
                                  0d0))
                         (transform (if-let (val (gethash "svg-transform" container-attributes))
                                        (gp:copy-transform val)
                                      (make-transform)))
                         (child (plump-dom:get-element-by-id root-node id))
                         ; Make a shadow tree
                         (shadow-child (make-instance 'plump:element
                                                      :parent node
                                                      :tag-name (plump:tag-name child)
                                                      :children (plump:make-child-array)
                                                      :attributes (plump:attributes node))))
                    (declare (type double-float new-x new-y))
                    ;; Move the left-top of the sub-graph to (x, y)
                    (apply-translation transform new-x new-y)
                    (setf (gethash "svg-transform" new-table) transform)
                    (maphash (lambda (key val)
                               (when (or (string= key "style")
                                         (member key *svg-presentation-attributes* :test #'string=))
                                 (plump:set-attribute shadow-child key val)))
                             (plump:attributes child))
                    (loop for child-child across (plump:children child)
                          for copy = (clos:copy-standard-object child-child)
                          do (plump-dom:append-child shadow-child copy))
                    (create-renderer port shadow-child root-node new-table)))
                 (t
                  (let ((funcs (delete nil (loop for child across (plump-dom:children node)
                                                 when (plump-dom:element-p child)
                                                   collect (create-renderer port child root-node new-table)))))
                    (lambda (port)
                      (dolist (func funcs)
                        (funcall func port)))))))
             ))))))

(defun draw-svg-from-string (port string x y)
  "Draw the first SVG element inside STRING to PORT.

This function does not cache the renderer, it will parse the SVG from
string each time the PORT redisplay, if you put it directly into the
`display-callback`. To improve the efficiency, you can cache the
result of `CREATE-RENDERER` and funcall it after each redisplay. See
the source code of this function about how to do."
  (let ((svg (first (plump:get-elements-by-tag-name (plump:parse string) "svg"))))
    (gp:with-graphics-translation (port x y)
      (let ((renderer (create-renderer port svg)))
        (funcall renderer port)))))
