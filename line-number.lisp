;; Copyright (c) 2025, April & May
;; SPDX-License-Identifier: 0BSD

;; Display Line Numbers Mode for LW Editor.

;; FIXME: Virtual strings will be count into tab-space while
;;        displaying, so line with tab will be corrupt when there's
;;        leading line number

(in-package editor)

(make-face 'line-number-face
           :foreground (create-dark-background-switchable-color :gray40 :gray60)
           :if-exists :overwrite)

(make-face 'highlighted-line-number-face
           :foreground (create-dark-background-switchable-color :black :white)
           :bold-p t
           :background (create-dark-background-switchable-color :gray80 :gray20)
           :if-exists :overwrite)

(define-editor-variable display-line-number-digits nil
   "The digits of the largest line number, used to format all display-line-numbers overlays.")

(defun display-line-number-after-change-function (buffer offset old new)
  (with-buffer-locked (buffer :for-modification nil)
    (with-point ((start (buffer-point buffer))
                 (end (buffer-point buffer)))
      (move-point-to-offset start offset)
      (move-point-to-offset end (max old new))
      (when (or (= offset 0)
                (null (find-next-character start))
                (point>= start end))
        (let* ((digits (1+ (floor (log (max 1 (buffer-newlines-count buffer)) 10))))
               (format-str (format nil " ~~~DD " digits))
               force-update)
          (when (not (eql digits (buffer-value buffer 'display-line-number-digits)))
            (setf (buffer-value buffer 'display-line-number-digits) digits
                  force-update t))
          (let (valid-overlays)
            (with-point ((point (buffers-start buffer)))
              (loop for line from 2
                    while (find-next-character point #\Newline)
                    do (with-point ((start point))
                         (point-after point)
                         (let ((overlays (loop for ov in (overlays-at start)
                                               when (overlay-get ov 'display-line-number)
                                                 if (point= (overlay-start ov) (overlay-end ov))
                                                   do (delete-overlay ov)
                                                 else collect ov)))
                           (if overlays
                             (progn
                               (when (> (length overlays) 1)
                                 (dolist (ov (cdr overlays))
                                   (delete-overlay ov)))
                               (let* ((ov (first overlays)))
                                 (when (or force-update
                                           (/= (overlay-get ov 'display-line-number) line))
                                   (overlay-put ov 'display-line-number line)
                                   (overlay-put ov 'after-string (cons (format nil format-str line) 'line-number-face)))
                                 (push ov valid-overlays)))
                             (let ((ov (make-overlay start point
                                                     :start-kind :after-insert
                                                     :end-kind :before-insert)))
                               (overlay-put ov 'display-line-number line)
                               (overlay-put ov 'after-string (cons (format nil format-str line) 'line-number-face))
                               (overlay-put ov 'priority -1)
                               (push ov valid-overlays)))))))
            (dolist (ov (buffer-overlays buffer))
              (when-let (num (overlay-get ov 'display-line-number))
                (unless (or (and (not force-update)
                                 (eql num 1)
                                 (point= (overlay-start ov) (buffers-start buffer)))
                            (memq ov valid-overlays))
                  (delete-overlay ov))))
            (let ((first (loop for ov in (overlays-at (buffers-start buffer))
                               when (overlay-get ov 'display-line-number)
                                 if (point= (overlay-start ov) (overlay-end ov))
                                   do (delete-overlay ov)
                                 else collect ov)))
              (unless first
                (when (point< (buffers-start buffer) (buffers-end buffer))
                  (with-point ((end (buffers-start buffer)))
                    (point-after end)
                    (let ((ov (make-overlay (buffers-start buffer) end
                                            :start-kind :temporary
                                            :end-kind :temporary)))
                      (overlay-put ov 'display-line-number 1)
                      (overlay-put ov 'before-string (cons (format nil format-str 1) 'line-number-face))
                      (overlay-put ov 'priority -1))))))))))))

(define-editor-variable highlighted-current-line-overlay nil)

(defun highlight-current-line-number-for-window (window)
  (let ((buffer (window-buffer window)))
    (when (buffer-minor-mode buffer "Display Line Numbers")
      (let* ((format-str (format nil " ~~~DD " (buffer-value buffer 'display-line-number-digits)))
             (old (buffer-value buffer 'highlighted-current-line-overlay))
             (old-num (when old (overlay-get old 'display-line-number))))
        (flet ((revert-old ()
                 (when old
                   (overlay-put old (if (eql old-num 1) 'before-string 'after-string)
                                (cons (format nil format-str old-num) 'line-number-face))
                   (overlay-put old 'display-line-number-highlighted nil))))
          (with-point ((point (buffer-point buffer)))
            (when (eql (character-at point 0) #\Newline)
              (point-before point))
            (if (find-previous-character point #\Newline)
              (when-let (ov (find-if (lambda (ov) (overlay-get ov 'display-line-number))
                                     (overlays-at point)))
                (let ((num (overlay-get ov 'display-line-number)))
                  (unless (eql old-num num)
                    (revert-old)
                    (overlay-put ov 'after-string (cons (format nil format-str num) 'highlighted-line-number-face))
                    (overlay-put ov 'display-line-number-highlighted t)
                    (setf (buffer-value buffer 'highlighted-current-line-overlay) ov))))
              (unless (eql old-num 1)
                (when-let (ov (find-if (lambda (ov) (eql (overlay-get ov 'display-line-number) 1))
                                       (buffer-overlays buffer)))
                  (revert-old)
                  (overlay-put ov 'before-string (cons (format nil format-str 1) 'highlighted-line-number-face))
                  (overlay-put ov 'display-line-number-highlighted t)
                  (setf (buffer-value buffer 'highlighted-current-line-overlay) ov))))))))))

(defmode "Display Line Numbers"
  :setup-function (lambda (buffer)
                    (let* ((digits (1+ (floor (log (max 1 (buffer-newlines-count buffer)) 10))))
                           (format-str (format nil " ~~~DD " digits)))
                      (setf (buffer-value buffer 'display-line-number-digits) digits)
                      (with-point ((end (buffers-start buffer)))
                        (point-after end)
                        (let ((ov (make-overlay (buffers-start buffer) end
                                                :start-kind :temporary
                                                :end-kind :temporary)))
                          (overlay-put ov 'display-line-number 1)
                          (overlay-put ov 'before-string (cons (format nil format-str 1) 'line-number-face))
                          (overlay-put ov 'priority -1)))
                      (with-point ((point (buffers-start buffer)))
                        (loop for i from 2
                              while (find-next-character point #\Newline)
                              do (with-point ((start point))
                                   (point-after point)
                                   (let ((ov (make-overlay start point
                                                           :start-kind :after-insert
                                                           :end-kind :before-insert)))
                                     (overlay-put ov 'display-line-number i)
                                     (overlay-put ov 'after-string (cons (format nil format-str i) 'line-number-face))
                                     (overlay-put ov 'priority -1))))))
                    (set-buffer-after-change-hook buffer 'display-line-number-after-change-function t))
  :cleanup-function (lambda (buffer)
                      (set-buffer-after-change-hook buffer 'display-line-number-after-change-function nil)
                      (dolist (ov (buffer-overlays buffer))
                        (when (overlay-get ov 'display-line-number)
                          (delete-overlay ov)))))

(defcommand "Display Line Numbers Mode" (p)
     "Toggle Display Line Numbers Mode"
     "Enable Display Line Numbers Mode when `p' is positive, otherwise disable it.
toggle the mode when `p' is nil."
  (let ((buffer (current-buffer)))
    (setf (buffer-minor-mode buffer "Display Line Numbers")
          (if p (plusp p) (not (buffer-minor-mode buffer "Display Line Numbers"))))))

(defvar *global-display-line-numbers-mode* nil)

(defun enable-display-line-numbers-mode (buffer &rest args)
  (declare (ignore args))
  (setf (editor:buffer-minor-mode buffer "Display Line Numbers") t))

(defcommand "Global Display Line Numbers Mode" (p)
     "" ""
  (let ((on-p (if p (plusp p) (not *global-display-line-numbers-mode*))))
    (cond (on-p
           (add-global-hook lisp-mode-hook 'enable-display-line-numbers-mode))
          (t
           (remove-global-hook lisp-mode-hook 'enable-display-line-numbers-mode)
           (dolist (buffer *buffer-list*)
             (when (buffer-minor-mode buffer "Display Line Numbers")
               (setf (buffer-minor-mode buffer "Display Line Numbers") nil)))))
    (message "Global Display Line Numbers mode %s." (if on-p "enabled" "disabled"))
    (setq *global-display-line-numbers-mode* on-p)))

(add-global-hook after-redisplay-hook 'highlight-current-line-number-for-window)
(global-display-line-numbers-mode-command 1)
