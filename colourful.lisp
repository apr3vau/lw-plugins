;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

;; Provides a more elaborate Lisp syntax highlighting to LispWorks Editor.
;; Features:
;;     Mark out functions, variables, package name and declarations;
;;     Mark out package of symbol before the colon;

;; Usage:
;;     Set the face configurations, load this file,
;;     the minor mode "Colourful" and editor command "Colourful Mode" will available.
;;
;;     The Colourful mode will be automatically enabled while opening Lisp files,
;;     If this is not what you want, comment the last form of this file.

(defpackage colourful
  (:use editor)
  (:add-use-defaults))
(in-package "COLOURFUL")

;; Face definitions

;; We define our own version of built-in faces here, according to our personal preference.
;; Defining your own if you want another style.
(make-face 'declaration-face
           :italic-p t
           :foreground :orange
           :if-exists :overwrite
           :documentation "Face for declarations, e.g. DECLARE, OPTIMIZE, IGNORE, INLINE, etc.")
(make-face 'special-operator-face
           :foreground (editor::create-dark-background-switchable-color :turquoise4 :darkslategray2)
           :bold-p t
           :if-exists :overwrite)
(make-face 'function-face
           :foreground (editor::create-dark-background-switchable-color :blue3 :lightblue)
           :if-exists :overwrite)
(make-face 'macro-face
           :foreground (editor::create-dark-background-switchable-color :slateblue3 :darkolivegreen2)
           :if-exists :overwrite)
(make-face 'type-face
           :foreground (editor::create-dark-background-switchable-color :forestgreen :burlywood2)
           :if-exists :overwrite)
(make-face 'builtin-face
           :italic-p t
           :foreground (editor::create-dark-background-switchable-color :orchid :pink)
           :if-exists :overwrite)
(make-face 'dynamic-variable-face
           :foreground (editor::create-dark-background-switchable-color :darkgoldenrod :lightgoldenrod)
           :if-exists :overwrite)
(make-face 'variable-name-face
           :foreground (editor::create-dark-background-switchable-color :aquamarine4 :aquamarine2)
           :if-exists :overwrite)

(export '(declaration-face builtin-face type-face function-face macro-face special-operator-face variable-face))

;; Symbol categories

(defvar *special-operators* 
  (loop for sym being each present-symbol of (find-package "CL")
        when (special-operator-p sym) collect sym))

(defvar *loop-keywords-names*
  '("named"
    "initially" "finally" "for" "as" "with"
    "do" "collect" "collecting" "append"
    "appending" "nconc" "nconcing" "into" "count"
    "counting" "sum" "summing" "maximize" "return" "loop-finish"
    "maximizing" "minimize" "minimizing" "doing"
    "thereis" "always" "never" "if" "when"
    "unless" "repeat" "while" "until"
    "=" "and" "it" "else" "end" "from" "upfrom"
    "above" "below" "to" "upto" "downto" "downfrom"
    "in" "on" "then" "across" "being" "each" "the" "hash-key"
    "hash-keys" "of" "using" "hash-value" "hash-values"
    "symbol" "symbols" "present-symbol"
    "present-symbols" "external-symbol"
    "external-symbols" "fixnum" "float" "of-type")
  "Loop keywords from https://lispcookbook.github.io/cl-cookbook/iteration.html#appendix-list-of-loop-keywords")

;; Utils

(defun apply-highlight (start end face)
  (declare (inline apply-highlight))
  (editor::font-lock-apply-highlight start end face))

(defun collect-forms (point)
  (with-point ((start point)
               (end point))
    (loop do (unless (form-offset end 1)
               (loop-finish))
             (move-point start end)
             (form-offset start -1)
       collect (list (copy-point start)
                     (copy-point end)))))

(defun count-success-quotes-before (point)
  (let ((buffer (point-buffer point))
        (count 0))
    (with-point ((start (buffers-start buffer)))
      (loop
         (unless (editor::find-string "\"" start) (return))
         (when (point> start point) (return))
         (when (evenp (loop for i downfrom -2
                            for c = (character-at start i)
                            until (not (eql c #\\))
                            count (eql c #\\)))
           (incf count))))
    count))

(defun try-skip-a-quote-backward (point)
  (loop until (and (eql (character-at point 0) #\")
                   (if (eql (character-at point -1) #\\)
                     (let ((count (loop for i downfrom -1
                                        for c = (character-at point i)
                                        until (not (eql c #\\))
                                        count (eql c #\\))))
                       (evenp count))
                     t))
        while (editor::point-before point)))

(defun try-skip-a-quote-forward (point)
  (loop until (and (eql (character-at point -1) #\")
                   (if (eql (character-at point -2) #\\)
                     (let ((count (loop for i downfrom -2
                                        for c = (character-at point i)
                                        until (not (eql c #\\))
                                        count (eql c #\\))))
                       (evenp count))
                     t))
        while (editor::point-after point)))

(defun skip-prefix (start)
  (loop for attr = (character-attribute :lisp-syntax (character-at start 0))
        while (eq attr :prefix)
        do (editor::point-after start))
  start)

;; Fontifying

(defun fontify-symbol (start end)
  (let* ((str (points-to-string start end))
         (split (split-sequence '(#\:) str))
         (symname (string-upcase (car (last split))))
         (sympak (or (when (> (length split) 1)
                       (find-package (if (string= (car split) "")
                                         "KEYWORD"
                                       (string-upcase (car split)))))
                     (editor::buffer-package-to-use start))))
    (multiple-value-bind (sym status)
        (find-symbol symname sympak)
      (let ((face
             (if status
               (cond ((member sym '(t nil))
                      'special-operator-face)
                     ((member sym *special-operators*)
                      'special-operator-face)
                     ((macro-function sym)
                      'macro-face)
                     ((eq (symbol-package sym) (find-package "KEYWORD"))
                      'builtin-face)
                     ((char= (schar str 0) #\&)
                      'special-operator-face)
                     ((or (find-class sym nil) (find-package sym)
                          (ignore-errors (subtypep sym t)))
                      'type-face)
                     ((fboundp sym)
                      'function-face)
                     ((boundp sym)
                      'dynamic-variable-face)
                     (t nil))
               (cond ((string= (car split) "")
                      'builtin-face)
                     ((find-package symname)
                      'type-face)))))
        (if (and (> (length split) 1)
                 (> (length (car split)) 0))
          (with-point ((p start))
            (character-offset p (+ (length (car split))
                                   (1- (length split))))
            (apply-highlight start p 'type-face)
            (when face (apply-highlight p end face)))
          (when face (apply-highlight start end face)))))))

(defun fontify-single-form (start end)
  "Fontify a single form, can be a symbol or a list, with prefix
characters.

This function is used to separate prefix & form, colouring prefix
characters, sending rest of the form to fontify-list or
fontify-symbol."
  (with-point ((point start))
    (when-let
        (attr (loop for char = (character-at point 0)
                    for attr = (character-attribute :lisp-syntax char)
                    while (or (eq attr :prefix)
                              ;; The default lisp-syntax counts #\@ as constituent instead of prefix,
                              ;; so we need to take additional consideration
                              (eql char #\@))
                    do (editor::point-after point)
                    finally (return attr)))
      (case attr
        (:open-paren
         (apply-highlight start point 'special-operator-face)
         (fontify-list point end))
        (:constituent
         (apply-highlight start point 'special-operator-face)
         (fontify-symbol point end))))))

(defun fontify-loop (lst)
  "Highlight loop keywords"
  (let ((1st (pop lst)))
    (apply-highlight (first 1st) (second 1st) 'macro-face))
  (dolist (form lst)
    (let* ((start (first form))
           (end (second form))
           (str (points-to-string start end)))
      (if (member str *loop-keywords-names* :test #'string-equal)
        (apply-highlight start end 'builtin-face)
        (fontify-single-form start end)))))

(defun fontify-declaration-list (lst)
  ;; FIXME: buggy?
  (let ((1st (pop lst)))
    (apply-highlight (first 1st) (second 1st) 'declaration-face))
  (dolist (form lst)
    (let* ((start (first form))
           (attr (loop for c = (character-at start 0)
                       for attr = (character-attribute :lisp-syntax c)
                       while (eq attr :prefix)
                       do (editor::point-after start)
                       finally (return attr))))
      (when (eq attr :open-paren)
        (progn
          (form-offset start 1 t -1)
          (let ((children (collect-forms start)))
            (when children (fontify-declaration-list children))))))))

(defun fontify-let (lst)
  "Highlight `let' form, or something behave like `let'."
  (let (1st)
    (destructuring-bind (start end) (pop lst)
      (fontify-single-form start end)
      (setq 1st (string-left-trim '(#\# #\' #\@ #\, #\`) (points-to-string start end))))
    (when lst
      (destructuring-bind (start end) (pop lst)
        (with-point ((prefix-end start))
          (skip-prefix prefix-end)
          (apply-highlight start prefix-end 'special-operator-face)
          (if (eql (character-at prefix-end 0) #\()
	    (when-let (sub (form-offset prefix-end 1 t -1))
	      (when-let (children (collect-forms sub))
		(if (or (member 1st '("dolist" "cl-dolist" "dotimes" "cl-dotimes"
                                      "do-symbols" "do-all-symbols")
                                :test #'string-equal)
			(and (member 1st '("when-let" "when-let*" "if-let" "if-let*")
                                     :test #'string-equal)
                             (with-point ((pt (caar children)))
                               (skip-prefix pt)
                               (not (eql (character-at pt 0) #\( )))))
                  (destructuring-bind (start end) (car children)
                    (apply-highlight start end 'variable-name-face)
                    (dolist (c (cdr children))
                      (apply #'fontify-single-form c)))
		  (dolist (child children)
		    (destructuring-bind (start end) child
                      (with-point ((prefix-end start))
                        (skip-prefix prefix-end)
                        (apply-highlight start prefix-end 'special-operator-face)
                        (if (eql (character-at prefix-end 0) #\()
                          (when-let (sub (form-offset prefix-end 1 t -1))
                            (when-let (children (collect-forms sub))
                              (destructuring-bind (start end)
                                  (car children)
                                (apply-highlight start end 'variable-name-face))
                              (dolist (c (cdr children))
                                (apply #'fontify-single-form c))))
                          (apply-highlight prefix-end end 'variable-name-face))))))))
            (fontify-symbol prefix-end end)))
	))
    (dolist (l lst)
      (apply #'fontify-single-form l))))

;; function arglist
(defun fontify-ordinary-lambda-list-at-1 (lst)
  "Highlight ordinary lambda list which is at the second of
expresion (typically lambda expression)."
  (destructuring-bind (start end) (pop lst)
    (fontify-single-form start end))
  (when lst
    (destructuring-bind (start end) (pop lst)
      (with-point ((prefix-end start))
        (skip-prefix prefix-end)
        (apply-highlight start prefix-end 'special-operator-face)
        (if (eql (character-at prefix-end 0) #\()
	  (when-let (sub (form-offset prefix-end 1 t -1))
	    (let ((children (collect-forms sub)))
	      (dolist (child children)
		(destructuring-bind (start end) child
                  (with-point ((prefix-end start))
                    (skip-prefix prefix-end)
                    (apply-highlight start prefix-end 'special-operator-face)
                    (case (character-at prefix-end 0)
                      (#\( (fontify-list prefix-end))
                      (#\& (apply-highlight start end 'special-operator-face))
                      (t (apply-highlight start end 'variable-name-face))))
		  ))))
          (fontify-symbol prefix-end end)))))
  (dolist (l lst)
    (apply 'fontify-single-form l)))

(defun fontify-ordinary-lambda-list-at-2 (lst)
  "Highlight ordinary lambda list which is at the third of
expresion (typically defun)."
  (destructuring-bind (start end) (pop lst)
    (fontify-single-form start end))
  (when lst (fontify-ordinary-lambda-list-at-1 lst)))

(defun fontify-list (start &optional end)
  "Parse items inside the list, sends them to corresponding fontify
functions."
  (declare (ignore end))
  (form-offset start 1 t -1)
  ;; Collect sub forms inside the list
  (when-let (forms (collect-forms start))
    (let ((1st (apply #'points-to-string (first forms))))
      ;; We can add conditions here, to apply custom fontify
      ;; function for specific clause
      (cond ((member 1st '("declare" "proclaim" "declaim")
                     :test #'string-equal)
             (fontify-declaration-list forms))
            ((string-equal 1st "loop")
             (fontify-loop forms))
            ((member 1st '("let" "let*" "when-let" "when-let*" "if-let" "if-let*"
                           "alexandria:when-let" "alexandria:when-let*" "alexandria:if-let" "alexandria:if-let*"
                           "prog" "prog*" "with-point" "editor:with-point"
                           "dolist" "cl-dolist" "dotimes" "cl-dotimes" "seq-doseq"
                           "do-symbols" "do-all-symbols" "cl-do-symbols" "cl-do-all-symbols"
                           "with-slots" "with-accessors")
                     :test #'string-equal)
             (fontify-let forms))
            ((member 1st '("lambda"
                              "multiple-value-bind" "cl-multiple-value-bind"
                              "destructuring-bind" "cl-destructuring-bind"
                              "with-gensyms" "with-unique-names")
                        :test #'string-equal)
             (fontify-ordinary-lambda-list-at-1 forms))
            ((member 1st '("defun" "defmacro" "defsubst" "defalias")
			:test #'string-equal)
             (fontify-ordinary-lambda-list-at-2 forms))
            (t (dolist (l forms) (apply #'fontify-single-form l)))))))

;; Main Function

(defun fontify-keywords-region (start end)
  "Set the font-lock-fontify-keywords-region-function to this function
to enable colourful mode."
  (with-buffer-locked ((point-buffer start) :for-modification nil)
    ;; Go outside string quotes if needed
    (when (oddp (count-success-quotes-before start))
      (try-skip-a-quote-backward start))
    (when (oddp (count-success-quotes-before end))
      (try-skip-a-quote-forward end))
    
    (with-point ((form-start end))
      (loop while (form-offset form-start -1 t 1)
            until (point< form-start start))
      (if (point< form-start start)
        ;; If the region is inside one form
        (with-point ((form-end form-start))
          (form-offset form-end 1)
          (fontify-single-form form-start form-end))
        ;; If the region has crossed the top-level
        (progn
          (move-point form-start start)
          (loop while (form-offset form-start -1 t 1))
          (with-point ((form-end form-start))
            (loop while (form-offset form-end 1)
                  do (move-point form-start form-end)
                     (form-offset form-start -1)
                     (fontify-single-form form-start form-end)
                  until (point> form-end end))))))))

(export 'fontify-keywords-region)

;; Editor Mode Definition

(defmode "Colourful"
  :vars '((editor::font-lock-fontify-keywords-region-function . fontify-keywords-region))
  :setup-function (lambda (buffer)
                    (when (editor::buffer-font-lock-mode-p buffer)
                      (editor::font-lock-fontify-buffer buffer))))

(setf (variable-value 'editor::font-lock-fontify-by-default :mode "Colourful") t)

(defcommand "Colourful Mode" (p)
     "Toggle Colourful Mode"
     "Enable Colourful mode when `p' is positive, otherwise disable it.
toggle the mode when `p' is nil."
  (let ((buffer (current-buffer)))
    (setf (buffer-minor-mode buffer "Colourful")
          (if p (plusp p) (not (buffer-minor-mode buffer "Colourful"))))))

(defun enable-colourful-mode (buffer &rest args)
  (declare (ignore args))
  (setf (buffer-minor-mode buffer "Colourful") t))

(add-global-hook lisp-mode-hook 'enable-colourful-mode)

(export '(*special-operators* *loop-keywords-names* colourful-mode-command enable-colourful-mode))
