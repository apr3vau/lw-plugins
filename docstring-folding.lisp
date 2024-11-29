;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

;; Here is a simple docstring folding facility for LispWorks Editor.

;; It support folding defun, defmethod, defvar, defparameter,
;; defgeneric, defmethod, defclass and its slots, and
;; editor:defcommand. Only top-level form will be folded.

;; The folded docstring will remain its first line and shows a "..."
;; ellipses at the end, same with the definition folding.

;; Recommend binding
;(bind-key "Toggle Current Docstring Folding" #("C-c" "C-d"))

;; To fold all definitions by default when opening lisp file,
;; add the following line to your ~/.lispworks
;(editor:add-global-hook editor:lisp-mode-hook 'editor::buffer-add-docstring-folding)

(in-package editor)

(defun docstring-folding-change-state (overlay invisible)
  (overlay-put overlay 'invisible invisible)
  (overlay-put overlay 'before-string (if invisible  "..." nil)))

(defun docstring-folding-overlay-make-visible (overlay)
  (when (overlay-get overlay '%%docstring-folding%%)
    (docstring-folding-change-state overlay nil)))

(defun docstring-folding-overlay-set-visible (overlay p)
  (when (overlay-get overlay '%%docstring-folding%%)
    (docstring-folding-change-state overlay p)))

(defun make-docstring-folding-overlay (start end)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay '%%docstring-folding%% t)
    (overlay-put overlay 'isearch-open-invisible 'docstring-folding-overlay-make-visible)
    (overlay-put overlay 'isearch-open-invisible-temporary 'docstring-folding-overlay-set-visible)
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'before-string "...")
    (overlay-put overlay 'face *font-lock-string-face*)
    overlay))

(defun docstring-folding-map-overlays (buffer func &rest args)
  (dolist (overlay (buffer-overlays buffer))
    (when (overlay-get overlay '%%docstring-folding%%)
      (apply func overlay args))))

(defun docstring-folding-fold-string (string-start)
  (with-point ((end-str string-start)
               (end-line string-start))
    (form-offset end-str 1)
    (line-end end-line)
    (unless (point>= end-line end-str)
      (make-docstring-folding-overlay end-line (point-before end-str)))))

(defun docstring-folding-fold-form (form-start)
  (with-point ((start form-start)
               (end form-start))
    (form-offset end 2 t -1)
    (move-point start end)
    (form-offset start -1)
    (let ((def (points-to-string start end)))
      (cond ((member def '(defun defmacro) :test #'string-equal)
             (loop for i from 0
                   do (setq end (form-offset end 1))
                   until (or (null end) (> i 3))
                   when (and (eql (character-at end -1) #\")
                             (member i '(2 3)))
                     do (form-offset end -1)
                        (docstring-folding-fold-string end)
                        (return)))
            ((string-equal def 'defmethod)
             (loop for i from 0
                   do (setq end (form-offset end 1))
                   until (or (null end) (> i 4))
                   when (and (eql (character-at end -1) #\")
                             (member i '(2 3 4)))
                     do (form-offset end -1)
                        (docstring-folding-fold-string end)
                        (return)))
            ((member def '("defcommand" "editor:defcommand") :test #'string-equal)
             (form-offset end 2)
             (loop while (whitespace-char-p (character-at end 0))
                   do (point-after end))
             (when (eql (character-at end 0) #\")
               (docstring-folding-fold-string end)
               (form-offset end 1)
               (loop while (whitespace-char-p (character-at end 0))
                     do (point-after end))
               (when (eql (character-at end 0) #\")
                 (docstring-folding-fold-string end))))
            ((member def '(defvar defparameter defconstant) :test #'string-equal)
             (setq end (form-offset end 3))
             (when end
               (form-offset end -1)
               (docstring-folding-fold-string end)))
            ((string-equal def 'defgeneric)
             (form-offset end 2)
             (loop (with-point ((sub-start end)
                                (sub-end end))
                     (unless (form-offset sub-end 2 t -1)
                       (return-from docstring-folding-fold-form))
                     (move-point sub-start sub-end)
                     (form-offset sub-start -1)
                     (cond ((string-equal (points-to-string sub-start sub-end)
                                          ":documentation") ; generic doc
                            (loop while (whitespace-char-p (character-at sub-end 0))
                                  do (point-after sub-end))
                            (docstring-folding-fold-string sub-end))
                           ((string-equal (points-to-string sub-start sub-end)
                                          ":method") ;method doc
                            (loop for i from 0
                                  do (setq sub-end (form-offset sub-end 1))
                                  until (or (null sub-end) (> i 3))
                                  when (and (eql (character-at sub-end -1) #\")
                                            (member i '(1 2 3)))
                                    do (form-offset sub-end -1)
                                       (docstring-folding-fold-string sub-end)
                                       (return)))))
                   (unless (form-offset end 1) (return))))
            ((string-equal def 'defclass)
             (form-offset end 2)
             ;; Slot docs
             (with-point ((pt end))
               (form-offset pt 1 t -1)
               (loop (loop while (whitespace-char-p (character-at pt 0))
                           do (point-after pt))
                     (when (eql (character-at pt 0) #\()
                       (with-point ((sub-start pt)
                                    (sub-end pt))
                         (form-offset sub-end 1 t -1)
                         (loop until (null (form-offset sub-end 1))
                               do (move-point sub-start sub-end)
                                  (form-offset sub-start -1)
                               when (string-equal (points-to-string sub-start sub-end)
                                                  ":documentation")
                                 do (loop while (whitespace-char-p (character-at sub-end 0))
                                          do (point-after sub-end))
                                    (docstring-folding-fold-string sub-end)
                                    (return))))
                     (unless (form-offset pt 1) (return))))
             ;; Class docs
             (form-offset end 1)
             (loop (loop while (whitespace-char-p (character-at end 0))
                         do (point-after end))
                   (when (eql (character-at end 0) #\()
                     (with-point ((sub-start end)
                                  (sub-end end))
                       (form-offset sub-end 2 t -1)
                       (move-point sub-start sub-end)
                       (form-offset sub-start -1)
                       (when (string-equal (points-to-string sub-start sub-end) ":documentation")
                         (loop while (whitespace-char-p (character-at sub-end 0))
                               do (point-after sub-end))
                         (docstring-folding-fold-string sub-end)
                         (return-from docstring-folding-fold-form))))
                   (unless (form-offset end 1) (return))))))))

(defun buffer-add-docstring-folding (buffer &rest args)
  (declare (ignore args)) ;for available as hook function
  (with-buffer-locked (buffer :for-modification nil)
    (docstring-folding-map-overlays buffer #'delete-overlay)
    (with-point ((point (buffers-start buffer)))
      (loop (unless (form-offset point 1) (return))
            (form-offset point -1)
            (docstring-folding-fold-form point)
            (unless (form-offset point 1) (return))))))

(defcommand "Toggle Current Docstring Folding" (p)
     "Toggle docstring folding in current top-level form."
     "Toggle docstring folding in current top-level form."
  (let* ((buffer (current-buffer))
         (point (buffer-point buffer))
         (count 0))
    (with-buffer-locked (buffer :for-modification nil)
      (with-point ((start point)
                   (end point))
        (get-defun-start-and-end-points point start end)
        (docstring-folding-map-overlays
         buffer
         (lambda (overlay)
           (let ((ov-start (overlay-start overlay)))
             (when (and (point<= start ov-start)
                        (point<= ov-start end))
               (docstring-folding-change-state
                overlay
                (if p (plusp p) (not (overlay-get overlay 'invisible))))
               (incf count)))))
        ;; Try to fold current form if there's no existed overlay in it
        (when (zerop count)
          (docstring-folding-fold-form point))))))

(defcommand "Fold Buffer Docstrings" (p)
     "Fold all docstrings in current buffer."
     "Fold all docstrings in current buffer."
  (declare (ignore p))
  (buffer-add-docstring-folding (current-buffer)))

(defcommand "Unfold Buffer Docstrings" (p)
     "Unfold all docstrings in current buffer."
     "Unfold all docstrings in current buffer."
  (declare (ignore p))
  (docstring-folding-map-overlays (current-buffer) 'docstring-folding-change-state nil))
