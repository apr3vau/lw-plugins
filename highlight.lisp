;; Copyright (c) 2025, April & May
;; SPDX-License-Identifier: 0BSD

;; Highlight facilities:
;; - Highlight current line
;; - Highlight current symbol
;;   - Jump to previous / next same symbol
;;     Default bindings: `C-<` and `C->`

(in-package editor)

;; Highlight current line

(make-face 'highlight-current-line-face
           :background (color:make-rgb 0.2 0.2 0.2)
           :if-exists :overwrite)

(define-editor-variable highlight-current-line-region nil
   "Buffer-local cache variable for the start and end point of
highlight-current-line face range, and the buffer-tick at previous
highlight.")

(defun highlight-current-line (&optional buffer &rest args)
  "Highlight the line with the current-point of BUFFER."
  (declare (ignore args))
  (let* ((buffer (or buffer (current-buffer)))
         (point (buffer-point buffer))
         (region (variable-value-if-bound 'highlight-current-line-region :buffer buffer)))
    (when region
      (destructuring-bind (start end tick) region
        (when (and (= tick (buffer-modified-tick buffer))
                   (same-line-p point start))
          (return-from highlight-current-line))
        (when (and (good-point-p start) (good-point-p end))
          (alter-text-property
           start end 'face
           (lambda (value)
             (if (listp value)
               (remove 'highlight-current-line-face value)
               (if (eq value 'highlight-current-line-face)
                 nil value)))
           :modification nil)
          (delete-point start)
          (delete-point end))))
    (let ((new-start (copy-point point))
          (new-end (copy-point point)))
      (move-to-column new-start 0)
      (unless (line-offset new-end 1 0)
        (setf (variable-value 'highlight-current-line-region :buffer buffer) nil)
        (return-from highlight-current-line))
      (alter-text-property
       new-start new-end 'face
       (lambda (value)
         (if (listp value)
           (append value (list 'highlight-current-line-face))
           (if value (list value 'highlight-current-line-face)
             'highlight-current-line-face)))
       :modification nil)
      (setf (variable-value 'highlight-current-line-region :buffer buffer)
            (list new-start new-end (buffer-modified-tick buffer))))))

(defmode "Highlight Current Line"
  :cleanup-function (lambda (buffer)
                      (when-let (region (variable-value-if-bound 'highlight-current-line-region :buffer buffer))
                        (destructuring-bind (start end tick) region
                          (declare (ignore tick))
                          (when (and (good-point-p start) (good-point-p end))
                            (alter-text-property
                             start end 'face
                             (lambda (value)
                               (if (listp value)
                                 (delete 'highlight-current-line-face value)
                                 (if (eq value 'highlight-current-line-face)
                                   nil value)))
                             :modification nil)
                            (delete-point start)
                            (delete-point end))
                          (setf (variable-value 'highlight-current-line-region :buffer buffer) nil)))))

(defun highlight-current-line-on-window (window)
  (let ((buffer (window-buffer window)))
    (when (buffer-minor-mode buffer "Highlight Current Line")
      (highlight-current-line buffer))))

(defcommand "Highlight Current Line Mode" (p)
     "Toggle Highlight Current Line Mode"
     "Enable Highlight Current Line mode when `p' is positive, otherwise disable it.
toggle the mode when `p' is nil."
  (let ((buffer (current-buffer)))
    (setf (buffer-minor-mode buffer "Highlight Current Line")
          (if p (plusp p) (not (buffer-minor-mode buffer "Highlight Current Line"))))))

(defun enable-highlight-current-line-mode (buffer &rest args)
  (declare (ignore args))
  (setf (editor:buffer-minor-mode buffer "Highlight Current Line") t))

(add-global-hook after-redisplay-hook 'highlight-current-line-on-window)
(add-global-hook lisp-mode-hook 'enable-highlight-current-line-mode)

;; Highlight current symbol

(make-face 'highlight-current-symbol-face
           :background (color:make-rgb 0.3 0.3 0.3)
           :underline-p t
           :if-exists :overwrite)

(define-editor-variable current-highlighted-symbol-string nil
   "Buffer local variable for the current highlighted symbol string.")

(defun search-all-matched-symbol-names (buffer string)
  "Search all (possibly) symbols that represent in STRING within BUFFER,
Return a list of (START END) points."
  (let ((pattern (get-search-pattern string :forward))
        result)
    (with-buffer-locked (buffer :for-modification nil)
      (with-point ((point (buffers-start buffer)))
        (loop for won = (i-find-pattern point pattern)
              while won
              do (let ((start (copy-point point)))
                   (in-character-offset point won)
                   (if (not (or (eq (character-attribute :lisp-syntax (character-at start -1)) :constituent)
                                (eq (character-attribute :lisp-syntax (character-at point 0)) :constituent)))
                     (push (list start (copy-point point)) result)
                     (delete-point start))))))
    result))

(defun highlight-current-symbol (&optional buffer &rest args)
  "Highlight the symbol under the current-point of BUFFER."
  (declare (ignore args))
  (let* ((buffer (or buffer (current-buffer)))
         (point (buffer-point buffer))
         (old-sym-str (variable-value-if-bound 'current-highlighted-symbol-string :buffer buffer))
         (sym-str (read-symbol-from-point :point point :read-package-name t :previous t)))
    (flet ((remove-old ()
             ;; Because the function can be triggered after modifications,
             ;; the highlighted positions after current edited point can be changed.
             ;; so we have to loop through the whole buffer to cancle previous
             ;; highlighted items.
             (when old-sym-str
               (alter-text-property
                (buffers-start buffer) (buffers-end buffer) 'face
                (lambda (value)
                  (if (listp value)
                    (remove 'highlight-current-symbol-face value)
                    (if (eq value 'highlight-current-symbol-face)
                      nil value)))
                :modification nil :remove-nil-p t))))
      (if (or (eq (character-attribute :lisp-syntax (character-at point -1)) :constituent)
              (eq (character-attribute :lisp-syntax (character-at point 0)) :constituent))
        (with-buffer-locked (buffer :for-modification nil)
          (if (equal old-sym-str sym-str)
            (return-from highlight-current-symbol)
            (remove-old))
          (when (and sym-str (> (length sym-str) 0))
            (setf (variable-value 'current-highlighted-symbol-string :buffer buffer) sym-str)
            (let ((regions (search-all-matched-symbol-names buffer sym-str)))
              (loop for (start end) in regions
                    do (alter-text-property
                        start end 'face
                        (lambda (value)
                          (if (listp value)
                            (append value (list 'highlight-current-symbol-face))
                            (if value (list value 'highlight-current-symbol-face)
                              'highlight-current-symbol-face)))
                        :modification nil)))))
        (progn
          (remove-old)
          (setf (variable-value 'current-highlighted-symbol-string :buffer buffer) nil))))))

(defmode "Highlight Current Symbol"
  :aliases 
  :cleanup-function (lambda (buffer)
                      (alter-text-property
                       (buffers-start buffer) (buffers-end buffer) 'face
                       (lambda (value)
                         (if (listp value)
                           (delete 'highlight-current-symbol-face value)
                           (if (eq value 'highlight-current-symbol-face)
                             nil value)))
                       :modification nil)
                      (setf (variable-value 'current-highlighted-symbol-string :buffer buffer) nil)))

(defun highlight-current-symbol-on-window (window)
  (let ((buffer (window-buffer window)))
    (when (buffer-minor-mode buffer "Highlight Current Symbol")
      (highlight-current-symbol buffer))))

(defcommand "Highlight Current Symbol Mode" (p)
     "Toggle Highlight Current Symbol Mode"
     "Enable Highlight Current Symbol mode when `p' is positive, otherwise disable it.
toggle the mode when `p' is nil."
  (let ((buffer (current-buffer)))
    (setf (buffer-minor-mode buffer "Highlight Current Symbol")
          (if p (plusp p) (not (buffer-minor-mode buffer "Highlight Current Symbol"))))))

(defun enable-highlight-current-symbol-mode (buffer &rest args)
  (declare (ignore args))
  (setf (editor:buffer-minor-mode buffer "Highlight Current Symbol") t))

(add-global-hook after-redisplay-hook 'highlight-current-symbol-on-window)
(add-global-hook lisp-mode-hook 'enable-highlight-current-symbol-mode)

;; Jump to same symbol

(defcommand "Jump To Next Same Symbol" (p &optional symbol-or-string)
     "Jump to the next occurence of the symbol currently under the point."
     "Jump to the next P occurence of the symbol, default to the symbol under the current-point."
  (let ((point (current-point)))
    (unless (or (eq (character-attribute :lisp-syntax (character-at point -1)) :constituent)
                (eq (character-attribute :lisp-syntax (character-at point 0)) :constituent))
      (editor-error "No symbol at point."))
    (let* ((sym-str (or (if (and symbol-or-string (symbolp symbol-or-string))
                          (let ((*package* (buffer-package-to-use point)))
                            (prin1-to-string symbol-or-string)))
                        symbol-or-string
                        (read-symbol-from-point :point point :read-package-name t :previous t)))
           (pattern (get-search-pattern sym-str :forward)))
      (with-point ((pt point))
        (when (eq (character-attribute :lisp-syntax (character-at pt 1)) :constituent)
          (form-offset pt 1))
        (loop repeat (or p 1)
              do (loop for won = (find-pattern pt pattern)
                       if won
                         do (let ((end (copy-point pt)))
                              (character-offset end won)
                              (if (not (or (eq (character-attribute :lisp-syntax (character-at pt -1)) :constituent)
                                           (eq (character-attribute :lisp-syntax (character-at end 0)) :constituent)))
                                (return pt)
                                (progn
                                  (move-point pt end)
                                  (delete-point end))))
                       else do (editor-error "No next same symbol.")))
        (move-point point pt)))))

(defcommand "Jump To Previous Same Symbol" (p &optional symbol-or-string)
     "Jump to the previous occurence of the symbol currently under the point."
     "Jump to the previous P occurence of the symbol, default to the symbol under the current-point."
  (let ((point (current-point)))
    (unless (or (eq (character-attribute :lisp-syntax (character-at point -1)) :constituent)
                (eq (character-attribute :lisp-syntax (character-at point 0)) :constituent))
      (editor-error "No symbol at point."))
    (let* ((sym-str (or (if (and symbol-or-string (symbolp symbol-or-string))
                          (let ((*package* (buffer-package-to-use point)))
                            (prin1-to-string symbol-or-string)))
                        symbol-or-string
                        (read-symbol-from-point :point point :read-package-name t :previous t)))
           (pattern (get-search-pattern sym-str :backward)))
      (with-point ((pt point))
        (when (eq (character-attribute :lisp-syntax (character-at pt -1)) :constituent)
          (form-offset pt -1))
        (loop repeat (or p 1)
              do (loop for won = (find-pattern pt pattern)
                       if won
                         do (let ((end (copy-point pt)))
                              (character-offset end won)
                              (if (not (or (eq (character-attribute :lisp-syntax (character-at pt -1)) :constituent)
                                           (eq (character-attribute :lisp-syntax (character-at end 0)) :constituent)))
                                (return pt)
                                (progn
                                  (delete-point end))))
                       else do (editor-error "No previous same symbol.")))
        (move-point point pt)))))

;; Recommend bindings
(bind-key "Jump To Previous Same Symbol" "C-<")
(bind-key "Jump To Next Same Symbol" "C->")
