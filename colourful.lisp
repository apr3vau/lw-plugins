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

(editor:make-face 'prefix-face
                  :bold-p t
                  :foreground (editor::create-dark-background-switchable-color :red :deeppink)
                  :if-exists :overwrite
                  :documentation "Face for characters that has attribute :PREFIX in :LISP-SYNTAX attributes")
(editor:make-face 'declaration-face
                  :italic-p t
                  :foreground :orange
                  :if-exists :overwrite
                  :documentation "Face for declarations, e.g. DECLARE, OPTIMIZE, IGNORE, etc.")

;; We define our own version of built-in faces here, according to our personal preference.
;; Defining your own if you want another style.
(editor:make-face 'font-lock-builtin-face
                  :bold-p t
                  :italic-p t
                  :foreground (editor::create-dark-background-switchable-color :orchid :pink)
                  :if-exists :overwrite)
(editor:make-face 'font-lock-type-face
                  :bold-p t
                  :italic-p t
                  :foreground (editor::create-dark-background-switchable-color :forestgreen :yellow)
                  :if-exists :overwrite)
(editor:make-face 'font-lock-function-name-face
                  :bold-p t
                  :foreground (editor::create-dark-background-switchable-color :blue :lightblue)
                  :if-exists :overwrite)
(editor:make-face 'font-lock-keyword-face
                  :bold-p t
                  :foreground (editor::create-dark-background-switchable-color :purple :light-red)
                  :if-exists :overwrite)

(export '(prefix-face declaration-face font-lock-builtin-face font-lock-type-face font-lock-function-name-face font-lock-keyword-face))

;; Symbol categories

(defvar *definition-symbols*
  (nconc (regexp-find-symbols "^def" :packages (find-package 'common-lisp))
         '(capi:define-interface capi:define-command capi:define-layout capi:define-menu)
         '(editor:defcommand editor:defattribute editor:defmode)
         (regexp-find-symbols "^define" :packages (list (find-package 'editor)
                                                        (find-package 'dspec))
                              :external-only t)
         '(dspec:def)
         '(call-next-method call-method))
  "Function / Macro symbols that are used to defining things.")

(defvar *builtin-symbols*
  (nconc (mapcar #'intern
                 (mapcar #'string-upcase
                         '("or" "and" "not"
                           "when" "unless" "cond" "if"
                           "case" "ecase" "typecase" "etypecase"
                           "loop" "loop-finish" "do" "do*" "dotimes" "dolist"
                           "do-symbols" "do-external-symbols" "do-all-symbols"
                           "proclaim" "declaim" "declare"
                           "flet" "labels" "macrolet"
                           "block" "return" "return-from"
                           "catch" "throw" "tagbody"
                           "handler-case" "handler-bind"
                           "restart-case" "restart-bind" "with-simple-restart"
                           "let" "let*" "prog" "destructuring-bind"
                           "multiple-value-bind" "multiple-value-prog1" "values"
                           "progn" "progv" "prog1" "prog2"
                           "inline" "unwind-protect"
                           "eval-when"
                           "with-slots" "with-accessors"
                           "with-input-from-string" "with-output-to-string"
                           "with-open-file" "with-open-stream"
                           "with-hash-table-iterator" "with-package-iterator"
                           "with-compilation-unit" "with-standard-io-syntax"
                           "pprint-logical-block" "print-unreadable-object")))
         *definition-symbols*)
  "Special symbols that should be colored in FONT-LOCK-BUILTIN-FACE.
We used the list present in EDITOR::*LISP-MODE-CONTROL-STRUCTURES-FSA*")

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
    (let* ((sym (find-symbol symname sympak))
           (face (when sym
                   (cond ((member sym '(t nil)) nil)
                         ((member sym *builtin-symbols*)
                          'font-lock-keyword-face)
                         ((eq (symbol-package sym) (find-package "KEYWORD"))
                          'font-lock-builtin-face)
                         ((char= (schar str 0) #\&)
                          'font-lock-type-face)
                         ((or (find-class sym nil) (find-package sym))
                          'font-lock-type-face)
                         ((or (fboundp sym) (macro-function sym))
                          'font-lock-function-name-face)
                         ((boundp sym)
                          'font-lock-variable-name-face)
                         (t nil)))))
      (if (and (> (length split) 1)
               (> (length (car split)) 0))
          (editor:with-point ((p start))
            (editor:character-offset p (+ (length (car split))
                                          (1- (length split))))
            (editor::font-lock-apply-highlight start p 'font-lock-type-face)
            (when face (editor::font-lock-apply-highlight p end face)))
        (when face (editor::font-lock-apply-highlight start end face))))))

(defun fontify-single-form (start end)
  (editor:with-point ((point start))
    (loop for attr = (editor:character-attribute :lisp-syntax (editor:character-at point 0))
          do (case attr
               (:prefix t)
               (:open-paren
                (editor::font-lock-apply-highlight start point 'prefix-face)
                (fontify-list point end)
                (return))
               (:constituent
                (editor::font-lock-apply-highlight start point 'prefix-face)
                (fontify-symbol point end)
                (return))
               (t (return)))
          do (editor::point-after point))))

(defun fontify-declaration-list (lst)
  (let ((1st (pop lst)))
    (editor::font-lock-apply-highlight
     (first 1st) (second 1st)
     'declaration-face))
  (dolist (form lst)
    (let ((start (first form))
          (end (second form)))
      (let ((attr (loop for c = (editor:character-at start 0)
                        for attr = (editor:character-attribute :lisp-syntax c)
                        until (not (eq attr :prefix))
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
  (declare (ignore end))
  (editor:with-point ((form-start start)
                      (form-end start))
    (editor:form-offset form-start 1 t -1)
    (editor:move-point form-end form-start)
    (let ((forms (loop do (unless (editor:form-offset form-end 1 t 0)
                            (loop-finish))
                          (editor:move-point form-start form-end)
                          (editor:form-offset form-start -1 t 0)
                       collect (list (editor:copy-point form-start :temporary)
                                     (editor:copy-point form-end :temporary)))))
      (when forms
        (cond ((member (apply #'editor:points-to-string (first forms))
                       '("declare" "proclaim" "declaim")
                       :test #'string-equal)
               (fontify-declaration-list forms))
              (t (dolist (l forms) (apply #'fontify-single-form l))))))))

;; Main Function

(defun fontify-keywords-region (start end)
  (editor:with-buffer-locked ((editor:point-buffer start) :for-modification nil)
    (editor::with-point ((form-start start)
                         (form-end start))
      (loop (unless (editor:form-offset form-end 1 t 0)
              (return))
            (editor:move-point form-start form-end)
            (editor:form-offset form-start -1 t 0)
            (fontify-single-form form-start form-end)
            (when (editor:point> form-end end) (return))))))

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

(editor:define-file-type-hook ("lispworks" "lisp" "lsp") (buffer type)
    (declare (ignore type))
    (setf (editor:buffer-major-mode buffer) "Lisp")
    (setf (editor:buffer-minor-mode buffer "Colourful") t))