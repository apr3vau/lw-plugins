;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

;; Provides SLY-style flex complete facility to LispWorks editor.
;; Provides additional key-binding for navigating complete items.

;; Provides 4 commands:
;; "Flex Complete Symbol":
;;     Flexible in-place symbol completion with fuzzy searching,
;;     similar with Sly.
;; "Indent Selection or Flex Complete Symbol"
;;     Similar with "Indent Selection or Complete Symbol", but using
;;     Flex Complete instead.
;; "Previous Completion" & "Next Completion":
;;     Offering a way to select in-place complete items using custom
;;     shortcut other than default Up / Down arrow key.

;; In addition, we provide a prefixing indicator for each complete
;; item, consist of 3 full-width characters, showing the bound
;; condition of the symbol:

;; The first character can be C / P, indicating if it's a class or
;; package name;
;; The second character can be F / G / M, indicating if it's a
;; function / generic function / macro;
;; The third character can be V / L, indicating if it has value /
;; plist.

;; This can be controlled by *FLEX-COMPLETE-ENABLE-INDICATOR-P*.

;; Usage:
;;   Load this file. The "Indent Selection or Flex Complete Symbol"
;;   will automatically bind to Tab, and "Previous Completion" /
;;   "Next Completion" will bind to M-p / M-n. Change the last part
;;   of this file to modify the default behavior.

(in-package editor)

(defvar *max-complete-items* 100
  "Maximum complete items being showed in in-place complete dialog

Keep this number lower will reduce graphical delay caused by building
list items.")

(defvar *flex-complete-enable-indicator-p* t
  "Whether enable the prefixing indicator of complete symbol.")

(defun flex-complete-fuzzy-search (target source)
  "Fuzzy search TARGET string within SOURCE.

If TARGET matches, return a list of the positions of each character of
TARGET in the SOURCE. Otherwise return NIL."
  (loop with start = -1
        for c across target
        if (> (1+ start) (length source))
          do (return)
        else do (setq start (position c source :start (1+ start) :test #'char-equal))
        if (null start) do (return)
        else collect start))

;; complete-in-place will call flex-complete-func & refresh candidate
;; list each time user change the selection (Up or Down). If the items
;; returned are not same with previous the completion panel will be
;; reset.

;; This may leads to a lot of recomputing when user navigating through
;; candidates and bring latency, so we make a cache here.
(defvar *flex-complete-prev-match* nil)

(defun flex-complete-func (str &optional arg)
  "Complete function for Flex Complete Symbol."
  (when (= (length str) 0) (return-from flex-complete-func))
  ;; Try to read our cache
  (when (equal str (car *flex-complete-prev-match*))
    (return-from flex-complete-func (cdr *flex-complete-prev-match*)))
  
  (let ((current-package (completion-arg-package arg))
        (split (split-sequence '(#\:) str))
        internal-packages external-packages
        (result (make-array 50 :fill-pointer 0 :adjustable t)))
    ;; Determine packages to search
    (case (length split)
      ;; No colon: current package's internals + other packages' (except KEYWORD) externals
      (1 (setq internal-packages (list current-package)
               external-packages (remove (find-package "KEYWORD") (list-all-packages))))
      ;; One colon: All packages' externals
      (2 (setq external-packages (list-all-packages)))
      ;; Two colons: Only matched package's internals
      (3 (setq internal-packages (loop with name = (string-upcase (first split))
                                       for package in (list-all-packages)
                                       when (flex-complete-fuzzy-search name (package-name package))
                                         collect package))))
    ;; Searching
    
    ;; In this step we find symbol that fuzzy-matching the STR, and
    ;; collect it with a embedded list structure: the SYM itself, a
    ;; list of matched position STARTS (returned by
    ;; FLEX-COMPLETE-FUZZY-SEARCH), and the string representation of
    ;; the symbol STRING. These informations will be used for sorting.
    (let ((*package* current-package))
      (dolist (package external-packages)
        (do-external-symbols (sym package)
          (let ((string (prin1-to-string sym)))
            (when-let (starts (flex-complete-fuzzy-search str string))
              (vector-push-extend (list sym starts string) result)))))
      (dolist (package internal-packages) 
       (let ((syms (system:package-internal-symbols package)))
          (dotimes (i (length syms))
            (let ((sym (aref syms i)))
              (when (symbolp sym)
                (let ((string (prin1-to-string sym)))
                  (when-let (starts (flex-complete-fuzzy-search str string))
                    (vector-push-extend (list sym starts string) result)))))))))
    ;; Sort symbols according to the "density" of matched characters -
    ;; more the matched characters grouped together, more it is
    ;; preferred. The start position of the first match is also count.
    ;; If they're same, then rank two symbols using length & string<
    ;; of their string representation.
    (sort result
          #'(lambda (list1 list2)
              (destructuring-bind (starts1 string1) list1
                (destructuring-bind (starts2 string2) list2
                  (let ((n1 (loop for j = 0 then i
                                  for i in starts1
                                  sum (- i j)))
                        (n2 (loop for j = 0 then i
                                  for i in starts2
                                  sum (- i j))))
                    (if (= n1 n2)
                      (let ((len1 (length string1))
                            (len2 (length string2)))
                        (if (= len1 len2)
                          (string< string1 string2)
                          (< len1 len2)))
                      (< n1 n2))))))
          :key #'cdr)
    ;; There may be some duplicate symbols during previous search, but
    ;; checking duplicate in a large vector is expensive, so we move
    ;; it here.
    (let ((final (delete-duplicates
                  (map 'list #'first (subseq result 0 (min *max-complete-items* (length result)))))))
      ;; Set cache
      (setq *flex-complete-prev-match* (cons str final))
      final)))

(defun flex-complete-symbol-completion-string-to-insert (res string package-and-case)
  "Modified version of SYMBOL-COMPLETION-STRING-TO-INSERT that
suitable for flex-completion.

Do not raise error when the prefixing package is not found, to allow
fuzzy package-name completion."
  (if (stringp res)
      res
    (let* ((package (completion-arg-package package-and-case))
           (case (if (> (count-if #'upper-case-p string)
                        (count-if #'lower-case-p string))
                   :upcase :downcase))
           (res-package (symbol-package res))
           (name (symbol-name res))
           (pname (let ((*package* res-package)
                        (*print-case* case))
                    (prin1-to-string res))))
      (if (or (keywordp res)
              (eq (find-symbol name package) res))
          pname
        (multiple-value-bind (old-package old-string old-external
                                          old-prefix-end)
            ;; Do not raise error when the prefixing package is not found,
            ;; to allow fuzzy package-name completion
            (pathetic-parse-symbol string package nil)
          (declare (ignore old-string))
          (if (and old-prefix-end
                   (eq res-package old-package))
              (string-append
               (subseq string 0
                       (if (and (eq (sys::find-external-symbol name res-package) res)
                                (not old-external))
                           (1- old-prefix-end)
                         old-prefix-end))
               pname)
            (let ((*package* package)
                  (*print-case* case))
              (prin1-to-string res))))))))

(defcommand "Flex Complete Symbol" (p)
     "Flexible in-place symbol completion with fuzzy searching, similar with Sly."
     "Flexible in-place symbol completion with fuzzy searching, similar with Sly."
  (declare (ignore p))
  (let ((package (buffer-package-to-use (current-point))))
    (complete-in-place
     'flex-complete-func
     :extract-func #'(lambda (point)
                       (let ((string (editor::read-symbol-from-point
                                      :point point :read-package-name t
                                      :previous t :fixed t)))
                         (character-offset point (- (length string)))
                         (values string (create-completion-arg :package package))))
     :skip-func #'(lambda (point) (move-point point (buffer-point (point-buffer point))))
     :insert-func 'flex-complete-symbol-completion-string-to-insert
     :tag 'complete-symbol
     :print-function (if *flex-complete-enable-indicator-p*
                         (lambda (x)
                           (let ((*package* package)
                                 (*print-case* *default-completion-case*))
                             (format nil "[~C~C~C] ~A"
                                     (cond ((find-class x nil) (code-char 65315))
                                           ((ignore-errors (subtypep x t)) (code-char 65332))
                                           ((find-package x) (code-char 65328))
                                           (t #\Ideographic-Space))
                                     (cond ((macro-function x) (code-char 65325))
                                           ((fboundp x)
                                            (if (typep (symbol-function x) 'generic-function)
                                              (code-char 65319) (code-char 65318)))
                                           (t #\Ideographic-Space))
                                     (cond ((boundp x) (code-char 65334))
                                           ((symbol-plist x) (code-char 65324))
                                           (t #\Ideographic-Space))
                                     (prin1-to-string x))))
                       (lambda (x)
                         (let ((*package* package)
                               (*print-case* *default-completion-case*))
                           (prin1-to-string x))))
     :after-insert-function 'complete-symbol-after-insert-function
     :terminating-chars *symbol-non-focus-completion-terminating-chars*)))

(defcommand "Indent Selection or Flex Complete Symbol" (p)
     "Either indent the current selection/line or flex-complete a symbol at the current point, according to where the point is in the line."
     "Either indent the current selection/line or flex-complete a symbol at the current point, according to where the point is in the line."
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
        (flex-complete-symbol-command p)))))

(defcommand "Next Completion" (p)
     "Select next in-place completion item."
     "Select next in-place completion item."
  (loop repeat (or p 1)
        do (pass-gesture-to-non-focus-completer
            (window-text-pane (current-window))
            :down)))

(defcommand "Previous Completion" (p)
     "Select previous in-place completion item."
     "Select previous in-place completion item."
  (loop repeat (or p 1)
        do (pass-gesture-to-non-focus-completer
            (window-text-pane (current-window))
            :up)))

(bind-key "Indent Selection or Flex Complete Symbol" #\Tab :mode "Lisp")
(bind-key "Previous Completion" "Meta-p" :mode "Lisp")
(bind-key "Next Completion" "Meta-n" :mode "Lisp")
