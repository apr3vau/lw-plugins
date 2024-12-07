;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

;; Markdown syntax highlighting for LispWorks Editor, Under the
;; specification of Github-Flavored Markdown.

;; What we does not support currently:
;; - Raw HTML
;; - Task list item
;; - Extended Autolinks

;; What we did not implement well:
;; - Autolinks (in pointy brackets) are lack of verification
;; - Tables are not parsed strictly
;; - Highlight region refreshment are not strict enough
;;   (sometimes will cause regions outdated)

;; Usage: Configure the font family & size, load this file,
;;        eval (EDITOR-MARKDOWN:UPDATE-FACE) after CAPI initialized.

;; 07Dec24:
;; - Fix wrongly extended code block region when editing under a code
;; block;
;; - Improve tilde / backquote code span recognition
;; - Inline code span now overrides other inlines correctly

(defpackage editor-markdown
  (:add-use-defaults))
(in-package editor-markdown)

(declaim (special *bold-font* *italic-font* *bold-italic-font* *strikethrough-font* *header-font* *mono-font* *mono-bold-font*))

;; Font family & size

(defvar *sans-serif-font-family-name*
  #+mswindows "Arial"
  #+macosx ".AppleSystemUIFont"
  #+linux "Liberation Sans")

(defvar *monospace-font-family-name*
  #+mswindows "Courier New"
  #+macosx "Monaco"
  #+linux "Liberation Mono")

(defvar *font-size* 12)
(defvar *header-font-size-increasement* 2)

;; Face

(defun update-face ()
  "Update face properties.

Should be called once after the CAPI has initialized, and after the
change of font family or size to adapt the change."
  (let ((port (capi:create-dummy-graphics-port)))
    (editor:make-face 'md-default-face
                      :font (gp:find-best-font port (gp:make-font-description :family *sans-serif-font-family-name* :size *font-size*))
                      :foreground nil :background nil
                      :if-exists :overwrite)
    (defparameter *bold-font*
      (gp:find-best-font port (gp:make-font-description :family *sans-serif-font-family-name* :size *font-size*
                                                        :weight :bold)))
    (defparameter *italic-font*
      (gp:find-best-font port (gp:make-font-description :family *sans-serif-font-family-name* :size *font-size*
                                                        :slant :italic)))
    (defparameter *bold-italic-font*
      (gp:find-best-font port (gp:make-font-description :family *sans-serif-font-family-name* :size *font-size*
                                                        :weight :bold :slant :italic)))
    (defparameter *strikethrough-font*
      (gp:find-best-font port (gp:make-font-description :family *sans-serif-font-family-name* :size *font-size*
                                                        :slant :italic)))
    (defparameter *header-font*
      (gp:find-best-font port (gp:make-font-description :family *sans-serif-font-family-name* :size (+ *font-size* *header-font-size-increasement*)
                                                        :weight :bold)))
    (defparameter *mono-font*
      (gp:find-best-font port (gp:make-font-description :family *monospace-font-family-name* :size *font-size*)))
    (defparameter *mono-bold-font*
      (gp:find-best-font port (gp:make-font-description :family *monospace-font-family-name* :size *font-size*
                                                        :weight :bold))))
  
  (editor:make-face 'md-code-block-face
                    :font *mono-font*
                    :background (editor::create-dark-background-switchable-color :lightcyan2 #(:RGB 0 0.2 0))
                    :if-exists :overwrite)
  (editor:make-face 'md-thematic-break-face
                    :background (editor::create-dark-background-switchable-color :grey70 :grey38)
                    :if-exists :overwrite)
  (editor:make-face 'md-header-face
                    :font *header-font*
                    :foreground (editor::create-dark-background-switchable-color :blue :lightblue)
                    :if-exists :overwrite)
  (editor:make-face 'md-list-face
                    :font *italic-font*
                    :foreground (editor::create-dark-background-switchable-color :darkgoldenrod :lightgoldenrod)
                    :if-exists :overwrite)
  (editor:make-face 'md-quote-face
                    :background (editor::create-dark-background-switchable-color :gray90 :gray20)
                    :foreground (editor::create-dark-background-switchable-color :red4 :pink)
                    :if-exists :overwrite)
  (editor:make-face 'md-link-face
                    :font *italic-font*
                    :foreground (editor::create-dark-background-switchable-color :dodgerblue :skyblue)
                    :if-exists :overwrite)
  (editor:make-face 'md-image-face
                    :font *bold-italic-font*
                    :foreground (editor::create-dark-background-switchable-color :dodgerblue :skyblue)
                    :if-exists :overwrite)
  (editor:make-face 'md-bold-face
                    :font *bold-font*
                    :foreground (editor::create-dark-background-switchable-color :orchid :orchid1)
                    :if-exists :overwrite)
  (editor:make-face 'md-italic-face
                    :font *italic-font*
                    :foreground (editor::create-dark-background-switchable-color :orchid :orchid1)
                    :if-exists :overwrite)
  (editor:make-face 'md-bold-italic-face
                    :font *bold-italic-font*
                    :foreground (editor::create-dark-background-switchable-color :orchid :orchid1)
                    :if-exists :overwrite)
  (editor:make-face 'md-strikethrough-face
                    :font *strikethrough-font*
                    :foreground (editor::create-dark-background-switchable-color :gray20 :gray80)
                    :background (editor::create-dark-background-switchable-color :gray80 :gray20)
                    :if-exists :overwrite)
  (editor:make-face 'md-code-span-face
                    :font *mono-font*
                    :foreground (editor::create-dark-background-switchable-color :darkgreen :palegreen)
                    :if-exists :overwrite)
  (editor:make-face 'md-table-face
                    :font *mono-font*
                    :foreground (editor::create-dark-background-switchable-color :cadetblue4 :cadetblue1)
                    :if-exists :overwrite)
  )

;; Util

(defun count-left-whitespaces (line)
  (let ((result 0))
    (loop for c across line
          do (cond ((eql c #\Tab) (incf result 4))
                   ((whitespace-char-p c) (incf result))
                   (t (return))))
    result))

(defun identical-char-length (text start)
  (if (> (length text) 0)
      (if (= start (1- (length text)))
          1
        (let ((char (char text start)))
          (loop for i from 1
                until (= (+ i start) (length text))
                for c = (char text (+ start i))
                until (not (eql c char))
                finally (return i))))
    nil))

;; Line-wise predicting function

;; We use these functions to find
;; - Headings
;; - Quote block
;; - Code block
;; - List item
;; - Thematic break

(defun line-atx-heading-p (line)
  (and (<= (count-left-whitespaces line) 3)
       (loop for i from 0
             for c across (string-trim-whitespace line)
             count (eql c #\#) into result
             until (not (eql c #\#))
             finally (return (and (or (whitespace-char-p c)
                                      (= i (length line)))
                                  (<= 1 result 6))))))

(defun line-quotes-p (line)
  (let ((trimed (string-trim-whitespace line)))
    (and (<= (count-left-whitespaces line) 3)
         (> (length trimed) 0)
         (eql (char trimed 0) #\>))))

(defun line-list-item-p (line)
  (let ((1st (first (split-sequence-if #'whitespace-char-p line :coalesce-separators t))))
    (and 1st
         (or (and (member (char 1st (1- (length 1st))) '(#\. #\)))
                  (< 1 (length 1st) 10)
                  (every #'digit-char-p (subseq 1st 0 (1- (length 1st)))))
             (and (= (length 1st) 1)
                  (member (char 1st (1- (length 1st))) '(#\+ #\- #\*)))))))

(defun line-setext-underline-p (line)
  (let ((trimed (string-trim-whitespace line)))
    (and (<= (count-left-whitespaces line) 3)
         (> (length trimed) 0)
         (or (every (lambda (c) (eql c #\=)) trimed)
             (every (lambda (c) (eql c #\-)) trimed)))))

(defun line-thematic-break-p (line)
  (and (<= (count-left-whitespaces line) 3)
       (or (every (lambda (c) (or (eql c #\*) (whitespace-char-p c))) line)
           (every (lambda (c) (or (eql c #\-) (whitespace-char-p c))) line)
           (every (lambda (c) (or (eql c #\_) (whitespace-char-p c))) line))
       (>= (count-if (lambda (c) (member c '(#\* #\- #\_))) line) 3)))

(defun line-indented-code-p (line)
  (>= (count-left-whitespaces line) 4))

(defun line-backquote-fence-p (line)
  (let* ((trimed (string-trim-whitespace line))
         (count (loop for c across trimed
                      if (eql c #\`) sum 1
                      else do (loop-finish))))
    (and (>= count 3)
         (<= (count-left-whitespaces line) 3))))

(defun line-tilde-fence-p (line)
  (let* ((trimed (string-trim-whitespace line))
         (count (loop for c across trimed
                      if (eql c #\~) sum 1
                      else do (loop-finish))))
    (and (>= count 3)
         (<= (count-left-whitespaces line) 3))))

(defun line-empty-p (line)
  (= (length (string-trim-whitespace line)) 0))

;; Multi-line predicting function

;; Giving a list of lines, find the pattern at the beginning of the
;; lines. If found, return a list of lines contained in the pattern.

;; We use these functions to find:
;; - Link definitions
;; - Tables

(defun find-link-def-at (lines start)
  (let ((line (nth start lines))
        label-start label-end dest-start dest-end title-start title-end title-in-quote-p
        (newline-count 0)
        (i 0))
    (loop (let ((char (if (= i (length line))
                          #\Newline
                        (char line i))))
            (cond ((null label-start) (if (eql char #\[)
                                          (setq label-start start)
                                        (return)))
                  ((null label-end) (cond ((eql char #\])
                                           (if (and (< (1+ i) (length line))
                                                    (eql (char line (1+ i)) #\:))
                                               (progn
                                                 (setq label-end start)
                                                 (incf i))
                                             (return)))
                                          ((member char '(#\Return #\Newline))
                                           (if (> newline-count 0)
                                               (return)
                                             (incf newline-count)))
                                          ((whitespace-char-p char) nil)
                                          (t (setq newline-count 0))))
                  ((null dest-start) (cond ((member char '(#\Return #\Newline))
                                            (if (> newline-count 0)
                                                (return)
                                              (incf newline-count)))
                                           ((whitespace-char-p char) nil)
                                           (t (setq newline-count 0
                                                    dest-start start))))
                  ((null dest-end) (cond ((whitespace-char-p char) (setq dest-end start))
                                         (t (setq newline-count 0))))
                  ((null title-start) (cond ((member char '(#\Return #\Newline))
                                             (if (> newline-count 0)
                                                 (return)
                                               (incf newline-count)))
                                            ((whitespace-char-p char) nil)
                                            (t (when (eql char #\')
                                                 (setq title-in-quote-p t))
                                               (setq newline-count 0
                                                     title-start start))))
                  ((null title-end) (cond ((eql char #\') (if title-in-quote-p
                                                              (setq title-end start)
                                                            (setq title-in-quote-p t)))
                                          ((whitespace-char-p char) (unless title-in-quote-p
                                                                      (setq title-end start)))))
                  (t (if (= start title-end)
                         (if (= (length (string-trim-whitespace (subseq line i (length line)))) 0)
                             (return)
                           (return-from find-link-def-at))
                       (return)))))
          (if (= i (length line))
              (progn
                (incf start)
                (setq line (nth start lines))
                (setq i 0)
                (when (= start (length lines))
                  (return))
                (when (or (line-quotes-p line) (line-atx-heading-p line)
                          (line-list-item-p line) (line-indented-code-p line))
                  (return)))
            (incf i)))
    (when dest-start
      (unless dest-end
        (setq dest-end (1- start)))
      (when title-start
        (unless title-end
          (setq title-end (1- start))))
      (loop for i from label-start to (or title-end dest-end)
            collect i))))

(defun find-table-at (lines start)
  (let (result header-row-p in-table-p)
    (loop for i from start to (1- (length lines))
          for line = (nth i lines)
          for trimed = (string-trim-whitespace line)
          for row-p = (and (> (length trimed) 0)
                           (eql (char trimed 0) #\|)
                           (eql (char trimed (1- (length trimed))) #\|))
          do (if row-p
               (cond (in-table-p (push i result))
                     (header-row-p
                      (if (every (lambda (c) (member c '(#\| #\: #\- #\Space))) trimed)
                          (progn
                            (setq in-table-p t
                                  header-row-p nil)
                            (push (1- i) result)
                            (push i result))
                        (setq header-row-p nil)))
                     (t (setq header-row-p t)))
               (return)))
    result))

;; Main search function

(defun search-lines (lines &optional tilde-fence-opening backquote-fence-opening)
  "Search through a list of lines, return a plist, each key
corresponding a kind of block, each value is a list of line numbers
that belong to this block."
  (let ((paragraph-lines 0)
        headings quotes list-items codes link-defs thematic-breaks tables
        paragraph-under-quote
        (i 0))
    (loop (let ((line (nth i lines))
                (link-def (find-link-def-at lines i))
                (table (find-table-at lines i)))
            (cond ((line-tilde-fence-p line) (if tilde-fence-opening
                                            (progn
                                              (push i codes)
                                              (setq tilde-fence-opening nil))
                                          (progn
                                            (setq paragraph-lines 0
                                                  paragraph-under-quote nil
                                                  tilde-fence-opening i)
                                            (push i codes))))
                  ((line-backquote-fence-p line) (if backquote-fence-opening
                                                (progn
                                                  (push i codes)
                                                  (setq backquote-fence-opening nil))
                                              (progn
                                                (setq paragraph-lines 0
                                                      paragraph-under-quote nil
                                                      backquote-fence-opening i)
                                                (push i codes))))
                  ((or tilde-fence-opening backquote-fence-opening) (push i codes))
                  ((line-atx-heading-p line) (progn
                                               (push i headings)
                                               (setq paragraph-lines 0
                                                     paragraph-under-quote nil)))
                  ((line-setext-underline-p line) (if (> paragraph-lines 0)
                                                      (loop for j from i downto (- i paragraph-lines)
                                                            do (push j headings))
                                                    (if (line-thematic-break-p line)
                                                        (progn
                                                          (push i thematic-breaks)
                                                          (setq paragraph-lines 0
                                                                paragraph-under-quote nil))
                                                      (incf paragraph-lines))))
                  ((line-thematic-break-p line) (progn
                                                  (push i thematic-breaks)
                                                  (setq paragraph-lines 0
                                                        paragraph-under-quote nil)))
                  (link-def (if (= paragraph-lines 0)
                                (progn
                                  (loop for j in link-def
                                        do (push j link-defs))
                                  (incf i (1- (length link-def))))
                              (incf paragraph-lines)))
                  (table (progn
                           (loop for j in table
                                 do (push j tables))
                           (incf i (1- (length table)))))
                  ((line-list-item-p line) (progn
                                             (setq paragraph-lines 0
                                                   paragraph-under-quote nil)
                                             (push i list-items)))
                  ((line-indented-code-p line) (if (eql paragraph-lines 0)
                                                   (push i codes)
                                                 (incf paragraph-lines)))
                  ((line-quotes-p line) (progn
                                          (setq paragraph-lines 1
                                                paragraph-under-quote t)
                                          (push i quotes)))
                  ((line-empty-p line) (setq paragraph-lines 0
                                             paragraph-under-quote nil))
                  (t (when paragraph-under-quote
                       (push i quotes))
                     (incf paragraph-lines)))
            (incf i)
            (when (= i (length lines))
              (return))
            (setq line (nth i lines))))
    (list :headings headings :quotes    quotes    :list-items list-items
          :codes    codes    :link-defs link-defs :breaks     thematic-breaks
          :tables   tables)))

(defun search-inlines (text)
  "Search through TEXT, return a plist, each key
corresponding a kind of inline-component, each value is a list of (START END) offsets
that contain the inline-component."
  (let (code-spans strikethroughs bolds italics bold-italics images links
                   
        single-code-span-opening double-code-span-opening
        single-strikethrough-opening double-strikethrough-opening
        
        *-italic-opening *-bold-opening *-bold-italic-opening
        _-italic-opening _-bold-opening _-bold-italic-opening
        
        image-opening link-title-opening link-title-closing link-dest-opening
        pointy-opening
        (i 0))
    (flet ((inline-code-override-any ()
             (declare (inline inline-code-override-any))
             (setq single-strikethrough-opening nil double-strikethrough-opening nil
                   *-italic-opening nil *-bold-opening nil *-bold-italic-opening nil
                   _-italic-opening nil _-bold-opening nil _-bold-italic-opening nil
                   image-opening nil link-title-opening nil link-title-closing nil
                   link-dest-opening nil)))
      (when (> (length text) 0)
        (loop (let ((char (char text i)))
                (case char
                  (#\\ (incf i))
                  (#\` (let ((len (identical-char-length text i)))
                         (case len
                           (1 (inline-code-override-any)
                              (if single-code-span-opening
                                (progn
                                  (push (list single-code-span-opening (1+ i)) code-spans)
                                  (setq single-code-span-opening nil))
                                (setq single-code-span-opening i)))
                           (2 (inline-code-override-any)
                              (if double-code-span-opening
                                (progn
                                  (push (list double-code-span-opening (+ 2 i)) code-spans)
                                  (setq double-code-span-opening nil))
                                (setq double-code-span-opening i))))
                         (incf i (1- len))))
                  (#\~ (unless (or single-code-span-opening double-code-span-opening)
                         (let ((len (identical-char-length text i)))
                           (case len
                             (1 (if single-strikethrough-opening
                                  (progn
                                    (push (list single-strikethrough-opening (1+ i)) strikethroughs)
                                    (setq single-strikethrough-opening nil))
                                  (setq single-strikethrough-opening i)))
                             (2 (if double-code-span-opening
                                  (progn
                                    (push (list double-strikethrough-opening (+ 2 i)) strikethroughs)
                                    (setq double-strikethrough-opening nil))
                                  (setq double-strikethrough-opening i))))
                           (incf i (1- len)))))
                  (#\* (unless (or single-code-span-opening double-code-span-opening)
                         (let ((len (identical-char-length text i)))
                           (case len
                             (1 (if *-italic-opening
                                  (progn
                                    (push (list *-italic-opening (1+ i)) italics)
                                    (setq *-italic-opening nil))
                                  (setq *-italic-opening i)))
                             (2 (if *-bold-opening
                                  (progn
                                    (push (list *-bold-opening (+ 2 i)) bolds)
                                    (setq *-bold-opening nil))
                                  (setq *-bold-opening i)))
                             (3 (if *-bold-italic-opening
                                  (progn
                                    (push (list *-bold-italic-opening (+ 3 i)) bold-italics)
                                    (setq *-bold-italic-opening nil))
                                  (setq *-bold-italic-opening i))))
                           (incf i (1- len)))))
                  (#\_ (unless (or single-code-span-opening double-code-span-opening)
                         (let ((len (identical-char-length text i)))
                           (case len
                             (1 (if _-italic-opening
                                  (progn
                                    (push (list _-italic-opening (1+ i)) italics)
                                    (setq _-italic-opening nil))
                                  (setq _-italic-opening i)))
                             (2 (if _-bold-opening
                                  (progn
                                    (push (list _-bold-opening (+ 2 i)) bolds)
                                    (setq _-bold-opening nil))
                                  (setq _-bold-opening i)))
                             (3 (if _-bold-italic-opening
                                  (progn
                                    (push (list _-bold-italic-opening (+ 3 i)) bold-italics)
                                    (setq _-bold-italic-opening nil))
                                  (setq _-bold-italic-opening i))))
                           (incf i (1- len)))))
                  (#\[ (setq link-title-opening i)
                       (if (and (> i 0)
                                (eql (char text (1- i)) #\!))
                         (setq image-opening (1- i))
                         (setq image-opening nil)))
                  (#\] (when link-title-opening
                         (unless (and (< i (1- (length text)))
                                      (eql (char text (1+ i)) #\())
                           (if image-opening
                             (push (list image-opening (1+ i)) images)
                             (push (list link-title-opening (1+ i)) links)))
                         (setq link-title-closing i)))
                  (#\< (setq pointy-opening i))
                  (#\> (when pointy-opening
                         (unless link-dest-opening
                           (push (list pointy-opening (1+ i)) links)))
                       (setq pointy-opening nil))
                  (#\( (unless pointy-opening
                         (when (eql link-title-closing (1- i))
                           (setq link-title-closing nil
                                 link-dest-opening i))))
                  (#\) (unless pointy-opening
                         (when link-dest-opening
                           (when link-title-opening
                             (if image-opening
                               (progn
                                 (push (list image-opening (1+ i)) images))
                               (progn
                                 (push (list link-title-opening (1+ i)) links)))
                             (setq image-opening nil
                                   link-title-opening nil
                                   link-dest-opening nil)))))
                  ((or #\Return #\Newline)
                   (setq single-code-span-opening nil double-code-span-opening nil
                         single-strikethrough-opening nil double-strikethrough-opening nil
                         *-italic-opening nil *-bold-opening nil *-bold-italic-opening nil
                         _-italic-opening nil _-bold-opening nil _-bold-italic-opening nil
                         image-opening nil link-title-opening nil link-title-closing nil
                         link-dest-opening nil pointy-opening nil))))
              (incf i)
              (when (= i (length text)) (return)))))
    (list :code-spans code-spans :strikethroughs strikethroughs
          :bolds      bolds      :italics        italics        :bold-italics bold-italics
          :images     images     :links          links)))

;; Font-lock Functions

(defun fontify-syntactically-region (start end)
  (editor::paragraph-offset start -1)
  (editor::paragraph-offset end 1)
  (editor:with-point ((s start)
                      (e end))
    (let* ((str (editor:points-to-string start end))
           (lines (split-sequence '(#\Newline) str))
           (prev-lines (split-sequence '(#\Newline)
                                       (editor:points-to-string
                                        (editor:buffers-start (editor:point-buffer start)) start)))
           (tilde-fence-opening (oddp (count-if #'line-tilde-fence-p prev-lines)))
           (backquote-fence-opening (oddp (count-if #'line-backquote-fence-p prev-lines)))
           (result (search-lines lines tilde-fence-opening backquote-fence-opening)))
      
      (editor::merge-face-property s e 'md-default-face :modification nil)
      (loop for key in '(:quotes :headings :list-items :codes :link-defs :breaks :tables)
            for face in '(md-quote-face md-header-face md-list-face md-code-block-face md-link-face md-thematic-break-face md-table-face)
            do (loop for prev-line = 0 then line
                     for line in (sort (getf result key) #'<)
                     do (editor:line-offset s (- line prev-line))
                        (editor:move-point e s)
                        (editor:line-end e)
                        (editor::merge-face-property s e face :modification nil))
               (editor:move-point s start)
               (editor:move-point e start))
      )))

(defun fontify-keywords-function (start end)
  (editor::paragraph-offset start -1)
  (editor::paragraph-offset end 1)
  (editor:with-point ((s start) (e end))
    (let* ((str (editor:points-to-string start end))
           (pos (editor:point-position start))
           (result (search-inlines str)))
      (loop for key in '(:code-spans :strikethroughs :bolds :italics :bold-italics :images :links)
            for face in '(md-code-span-face md-strikethrough-face md-bold-face md-italic-face md-bold-italic-face md-image-face md-link-face)
            do (loop for (pos-start pos-end) in (getf result key) 
                     do (setf (editor:point-position s) (+ pos pos-start)
                              (editor:point-position e) (+ pos pos-end))
                        (unless (or (member 'md-code-block-face (editor:get-text-property s 'editor:face))
                                    (member 'md-code-block-face (editor:get-text-property e 'editor:face)))
                          (editor::merge-face-property s e face :modification nil)
                          (editor:move-point s start)
                          (editor:move-point e start)))))))

;; Mode definition

(editor::defmode "Markdown"
  :major-p t
  :vars '((editor::font-lock-fontify-syntactically-region-function
           . fontify-syntactically-region)
          (editor::font-lock-fontify-keywords-region-function
           . fontify-keywords-function)))

(setf (editor:variable-value 'editor::font-lock-fontify-by-default :mode "Markdown") t)

(editor:add-global-hook editor::markdown-mode-hook #'editor:turn-on-font-lock)

(editor:defcommand "Markdown Mode" (p)
     "Enable Markdown Mode."
     "Enable Markdown Mode."
  (let ((buffer (editor:current-buffer)))
    (when (if p (plusp p) (not (equal (editor:buffer-major-mode buffer) "Markdown")))
      (setf (editor:buffer-major-mode buffer) "Markdown"))))

(editor:define-file-type-hook
    ("md" "markdown")
    (buffer type)
  (declare (ignore type))
  (setf (editor:buffer-major-mode buffer) "Markdown"))

;; For the use within IDE

(define-action "Initialize LispWorks Tools" "Initialize Editor-Markdown"
               (lambda (&rest args)
                 (declare (ignore args))
                 (update-face))
               :after "Create default the tools")

(export '(*sans-serif-font-family-name*
          *monospace-font-family-name*
          *font-size*
          *header-font-size-increasement*
          md-link-face
          md-bold-face
          md-list-face
          md-quote-face
          md-table-face
          md-image-face
          md-header-face
          md-italic-face
          md-default-face
          md-strikeout-face
          md-code-span-face
          md-code-block-face
          md-bold-italic-face
          md-strikethrough-face
          update-face
          fontify-syntactically-region
          fontify-keywords-function))
