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
  (:add-use-defaults))
(in-package "COLOURFUL")

;; Face definitions

;; We define our own version of built-in faces here, according to our personal preference.
;; Defining your own if you want another style.
(editor:make-face 'declaration-face
                  :italic-p t
                  :foreground :orange
                  :if-exists :overwrite
                  :documentation "Face for declarations, e.g. DECLARE, OPTIMIZE, IGNORE, INLINE, etc.")
(editor:make-face 'special-operator-face
                  :foreground (editor::create-dark-background-switchable-color :turquoise4 :darkslategray2)
                  :bold-p t
                  :if-exists :overwrite)
(editor:make-face 'function-face
                  :foreground (editor::create-dark-background-switchable-color :blue3 :lightblue)
                  :if-exists :overwrite)
(editor:make-face 'macro-face
                  :foreground (editor::create-dark-background-switchable-color :slateblue3 :darkolivegreen2)
                  :if-exists :overwrite)
(editor:make-face 'type-face
                  :foreground (editor::create-dark-background-switchable-color :forestgreen :burlywood2)
                  :if-exists :overwrite)
(editor:make-face 'builtin-face
                  :italic-p t
                  :foreground (editor::create-dark-background-switchable-color :orchid :pink)
                  :if-exists :overwrite)
(editor:make-face 'variable-face
                  :foreground (editor::create-dark-background-switchable-color :darkgoldenrod :lightgoldenrod)
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

;; Fontifying

(defun fontify-symbol (start end)
  (let* ((str (editor:points-to-string start end))
         (split (split-sequence '(#\:) str))
         (symname (string-upcase (car (last split))))
         (sympak (or (when (> (length split) 1)
                       (find-package (if (string= (car split) "")
                                         "KEYWORD"
                                       (string-upcase (car split)))))
                     (editor::buffer-package-to-use start))))
    (multiple-value-bind (sym status)
        (find-symbol symname sympak)
      (let ((face (if status
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
                           'variable-face)
                          (t nil))
                    (cond ((string= (car split) "")
                           'builtin-face)
                          ((find-package symname)
                           'type-face)))))
        (if (and (> (length split) 1)
                 (> (length (car split)) 0))
          (editor:with-point ((p start))
            (editor:character-offset p (+ (length (car split))
                                          (1- (length split))))
            (editor::font-lock-apply-highlight start p 'type-face)
            (when face (editor::font-lock-apply-highlight p end face)))
          (when face (editor::font-lock-apply-highlight start end face)))))))

(defun fontify-single-form (start end)
  "Fontify a single form, can be a symbol or a list, with prefix
(reader macro) characters.

This function is used to separate prefix & form, colouring prefix
characters, sending rest of the form to fontify-list or
fontify-symbol."
  (editor:with-point ((point start))
    (loop for char = (editor:character-at point 0)
          for attr = (editor:character-attribute :lisp-syntax char)
          do (case attr
               (:prefix)
               (:open-paren
                (editor::font-lock-apply-highlight start point 'special-operator-face)
                (fontify-list point end)
                (return))
               (:constituent
                ;; The default lisp-syntax counts #\@ as constituent instead of prefix,
                ;; so we need to take additional consideration
                (unless (eql char #\@)
                  (editor::font-lock-apply-highlight start point 'special-operator-face)
                  (fontify-symbol point end)
                  (return)))
               (t (return)))
          do (editor::point-after point))))

(defun fontify-loop (lst)
  "Highlight loop keywords"
  (let ((1st (pop lst)))
    (editor::font-lock-apply-highlight
     (first 1st) (second 1st)
     'macro-face))
  (dolist (form lst)
    (let* ((start (first form))
           (end (second form))
           (str (editor:points-to-string start end)))
      (if (member str *loop-keywords-names* :test #'string-equal)
        (editor::font-lock-apply-highlight start end 'builtin-face)
        (fontify-single-form start end)))))

(defun fontify-declaration-list (lst)
  ;; FIXME: buggy?
  (let ((1st (pop lst)))
    (editor::font-lock-apply-highlight
     (first 1st) (second 1st)
     'declaration-face))
  (dolist (form lst)
    (let ((start (first form))
          (end (second form)))
      (let ((attr (loop for c = (editor:character-at start 0)
                        for attr = (editor:character-attribute :lisp-syntax c)
                        while (eq attr :prefix)
                        do (editor::point-after start)
                        finally (return attr))))
        (when (eq attr :open-paren)
          (progn
            (editor:form-offset start 1 t -1)
            (editor:move-point end start)
            (let ((children (loop do (unless (editor:form-offset end 1 t 0)
                                       (loop-finish))
                                     (editor:move-point start end)
                                     (editor:form-offset start -1 t 0)
                               collect (list (editor:copy-point start :temporary)
                                             (editor:copy-point end :temporary)))))
              (when children (fontify-declaration-list children)))))))))

(defun fontify-list (start &optional end)
  "Parse items inside the list, sends them to corresponding fontify
functions."
  (declare (ignore end))
  (editor:with-point ((form-start start)
                      (form-end start))
    (editor:form-offset form-start 1 t -1)
    (editor:move-point form-end form-start)
    ;; Collect sub forms inside the list
    (let ((forms (loop do (unless (editor:form-offset form-end 1 t 0)
                            (loop-finish))
                          (editor:move-point form-start form-end)
                          (editor:form-offset form-start -1 t 0)
                       collect (list (editor:copy-point form-start :temporary)
                                     (editor:copy-point form-end :temporary)))))
      (when forms
        (let ((1st (apply #'editor:points-to-string (first forms))))
          ;; We can add conditions here, to apply custom fontify
          ;; function for specific clause
          (cond ((member 1st '("declare" "proclaim" "declaim")
                         :test #'string-equal)
                 (fontify-declaration-list forms))
                ((string-equal 1st "loop")
                 (fontify-loop forms))
                (t (dolist (l forms) (apply #'fontify-single-form l)))))))))

;; Main Function

(defun fontify-keywords-region (start end)
  "Set the font-lock-fontify-keywords-region-function to this function
to enable colourful mode."
  (editor:with-buffer-locked ((editor:point-buffer start) :for-modification nil)
    (editor:with-point ((form-start end))
      (loop while (editor:form-offset form-start -1 t 1)
            until (editor:point< form-start start))
      (if (editor:point< form-start start)
        ;; If the region is inside one form
        (editor:with-point ((form-end form-start))
          (editor:form-offset form-end 1)
          (fontify-single-form form-start form-end))
        ;; If the region has crossed the top-level
        (progn
          (editor:move-point form-start start)
          (loop while (editor:form-offset form-start -1 t 1))
          (editor:with-point ((form-end form-start))
            (loop while (editor:form-offset form-end 1)
                  do (editor:move-point form-start form-end)
                     (editor:form-offset form-start -1)
                     (fontify-single-form form-start form-end)
                  until (editor:point> form-end end))))))))

(export 'fontify-keywords-region)

;; Editor Mode Definition

(editor:defmode "Colourful"
  :vars '((editor::font-lock-fontify-keywords-region-function . fontify-keywords-region))
  :setup-function (lambda (buffer)
                    (when (editor::buffer-font-lock-mode-p buffer)
                      (editor::font-lock-fontify-buffer buffer))))

(setf (editor:variable-value 'editor::font-lock-fontify-by-default :mode "Colourful") t)

(editor:defcommand "Colourful Mode" (p)
     "Toggle Colourful Mode"
     "Enable Colourful mode when `p' is positive, otherwise disable it.
toggle the mode when `p' is nil."
  (let ((buffer (editor:current-buffer)))
    (setf (editor:buffer-minor-mode buffer "Colourful")
          (if p (plusp p) (not (editor:buffer-minor-mode buffer "Colourful"))))))

(defun enable-colourful-mode (buffer &rest args)
  (declare (ignore args))
  (setf (editor:buffer-minor-mode buffer "Colourful") t))

(editor:add-global-hook editor:lisp-mode-hook 'enable-colourful-mode)

(export '(*special-operators* *loop-keywords-names* colourful-mode-command enable-colourful-mode))
