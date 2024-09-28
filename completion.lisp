(in-package editor)

(unless (find-package :cl-ppcre)
  (ql:quickload :cl-ppcre))

(defun print-sym (x package)
  (let ((*package* package)
        (*print-case* *default-completion-case*))
    (prin1-to-string x)))

(defun enhance-complete-func (str &optional arg)
  (when (= (length str) 0) (return-from enhance-complete-func))
  (let* ((split (split-sequence '(#\:) str))
         (current-pak (completion-arg-package arg))
         ;; Determine packages to search
         (internal-pak (append (list current-pak)
                               (when (= (length split) 3)
                                 (loop with n = (string-upcase (car split))
                                       for p in (list-all-packages)
                                       when (search n (package-name p))
                                         collect p))))
         (external-pak (unless (= (length split) 3)
                         (remove-if (lambda (p)
                                      (member p (if (= (length split) 1)
                                                    (cons (find-package "KEYWORD") internal-pak)
                                                  internal-pak)))
                                    (list-all-packages))))
         ;; Build regex and scan
         (regex (ppcre:create-scanner
                 (loop for c across str
                       collect `(:sequence (:register ,c)
                                 (:greedy-repetition 0 nil :everything))
                         into r
                       finally (return (list :group :case-insensitive-p
                                             (cons :sequence r))))))
         (infos (let (r)
                  (loop for pak in external-pak
                        do (loop for sym being each external-symbol of pak
                                 for (start nil starts)
                                   = (multiple-value-list
                                      (ppcre:scan regex (print-sym sym current-pak)))
                                 when (and (eq (symbol-package sym) pak)
                                           start)
                                   do (push (list sym starts) r)))
                  (loop for pak in internal-pak
                        do (loop for sym being each present-symbol of pak
                                 for (start nil starts)
                                   = (multiple-value-list
                                      (ppcre:scan regex (print-sym sym current-pak)))
                                 when start
                                   do (pushnew (list sym starts) r
                                               :test #'(lambda (i1 i2)
                                                         (eq (car i1) (car i2))))))
                  r))
         ;; Sort results according to "density" of matched characters
         (sorted (sort infos
                       #'(lambda (inf1 inf2)
                           (let ((n1 (loop for j = 0 then i
                                           for i across (second inf1)
                                           sum (- i j)))
                                 (n2 (loop for j = 0 then i
                                           for i across (second inf2)
                                           sum (- i j))))
                             (if (= n1 n2)
                                 (< (length (symbol-name (first inf1)))
                                    (length (symbol-name (first inf2))))
                               (< n1 n2)))))))
    (loop for (sym) in (subseq sorted 0 (min 50 (length sorted)))
          collect sym)))

(defcommand "Enhance Complete Symbol" (p)
     "" ""
  (declare (ignore p))
  (let ((package (buffer-package-to-use (current-point))))
    (editor:complete-in-place
     'enhance-complete-func
     :extract-func #'(lambda (point)
                       (let ((string (editor::read-symbol-from-point
                                      :point point :read-package-name t
                                      :previous t :fixed t)))
                         (character-offset point (- (length string)))
                         (values string (create-completion-arg :package package))))
     :skip-func #'(lambda (point) (move-point point (buffer-point (point-buffer point))))
     :insert-func 'symbol-completion-string-to-insert
     :tag 'complete-symbol
     :print-function (lambda (x)
                       (let ((*package* package)
                             (*print-case* *default-completion-case*))
                         (format nil "[~A~A~A] ~A"
                                 (or (and (find-class x nil) "Ｃ") (and (find-package x) "Ｐ") "　")
                                 (or (and (fboundp x) (if (typep (symbol-function x) 'generic-function) "Ｇ" "Ｆ"))
                                     (and (macro-function x) "Ｍ")
                                     "　")
                                 (or (and (boundp x) "Ｖ") "　")
                                 (prin1-to-string x))))
     :after-insert-function 'complete-symbol-after-insert-function
     :terminating-chars *symbol-non-focus-completion-terminating-chars*)))

(defcommand "Indent Selection or Enhance Complete Symbol" (p)
     "" ""
  (if (region-highlighted-p (current-buffer))
      (indent-selection-command p)
    (let ((point (current-point))
          (completep (eq (last-command-type) :indent-or-complete-symbol)))
      (unless completep
        (let ((position (point-position point)))
          (editor:indent-command p)
          (setf (last-command-type) :indent-or-complete-symbol)
          (when (= position (point-position point))
            (setq completep t))))
      (when (and completep
                 (eq (character-attribute :lisp-syntax (character-at point -1))
                     :constituent))
        (enhance-complete-symbol-command p)))))

(defcommand "Next Completion" (p)
     "" ""
  (loop repeat (or p 1)
        do (pass-gesture-to-non-focus-completer 
            (window-text-pane (current-window))
            :down)))

(defcommand "Previous Completion" (p)
     "" ""
  (loop repeat (or p 1)
        do (pass-gesture-to-non-focus-completer 
            (window-text-pane (current-window))
            :up)))

(bind-key "Indent Selection or Enhance Complete Symbol" #\Tab :mode "Lisp")
(bind-key "Previous Completion" "Meta-p" :mode "Lisp")
(bind-key "Next Completion" "Meta-n" :mode "Lisp")
