;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

;; Display Nerd icons for Directory mode files.

(in-package editor)

;; Trying to respect the settings :(
;; We use symbol's plist to indicate if our offset has been added.
(declaim (special *directory-mode-name-preceding-offset*))
(unless (get '*directory-mode-name-preceding-offset* 'nerd-icons:nerd-icons)
  (if (and (boundp '*directory-mode-name-preceding-offset*)
           *directory-mode-name-preceding-offset*)
      (incf *directory-mode-name-preceding-offset* 2)
    (defparameter *directory-mode-name-preceding-offset* 13)) 
  (setf (get '*directory-mode-name-preceding-offset* 'nerd-icons:nerd-icons) t))

;; We produce lists of (string size icon-sym color) instead of (string . size) here
(defadvice (add-string-size-pair nerd-icons :around) (vector name fdf-handle suffixes spec)
  (when (and (or (not spec)
                 (sys::wild-inferior-comp-match-p spec name))
             (or (not suffixes)
                 (not (directory-mode-check-name-in-sufixes name suffixes))))
    (let* ((string (if (hcl::fdf-handle-directory-p fdf-handle)
                      (string-append name "/")
                    name))
          (fullname (string-append (fdf-handle-directory-string fdf-handle) string)))
      (multiple-value-bind (icon color) (nerd-icons:file-to-icon-name fullname)
        (vector-push-extend (list string (hcl::fdf-handle-size fdf-handle) icon color) vector)))))

(defadvice (insert-directory-mode-string-size-pairs nerd-icons :around) (point string-size-pairs)
  (dotimes (index (fill-pointer string-size-pairs))
    (let ((pair (aref string-size-pairs index))
          (pane (window-text-pane (current-window)))
          string size icon color)
      (if (listp (cdr pair))
          (setq string (first pair) size (second pair)
                icon (third pair) color (fourth pair))
        (setq string (car pair) size (cdr pair)))
      (flet ((print-size (num)
               (if (numberp num)
                   (cond ((< num 1024)
                          (format nil "~dB" num))
                         ((< num 1048576)
                          (format nil "~,1fK" (/ num 1024)))
                         ((< num 1073741824)
                          (format nil "~,1fM" (/ num 1048576)))
                         (t (format nil "~,1fG" (/ num 1073741824))))
                 "")))
        (insert-buffer-string
         point
         (make-buffer-string
          :%string (format nil "~A~7<~;~A~> ~C ~A~%"
                           *directory-mode-prefix*
                           ;; Respecting what we did in lw-plugins/directory.lisp
                           (if (and (boundp '*directory-mode-print-human-readable-size*)
                                    *directory-mode-print-human-readable-size*)
                               (print-size size) size)
                           (or (get icon :char) #\Space)
                           string)
          :properties (list (list (1- *directory-mode-name-preceding-offset*)
                                  *directory-mode-name-preceding-offset*
                                  (list 'face (make-face nil
                                                         :font (gp:find-best-font pane (gp:make-font-description :family nerd-icons:*nerd-font-family*))
                                                         :size (gp:font-description-attribute-value
                                                                (gp:font-description (capi:simple-pane-font pane))
                                                                :size)
                                                         :foreground (color:get-color-translation color)))))))))))

;; We increased the prefix length, so we should redefine this
;; function. Although they looks same, the new one here will use the
;; variable *DIRECTORY-MODE-NAME-PRECEDING-OFFSET* instead of the
;; built-in shaked constant.
(defadvice (string-directory-mode-name-index nerd-icons :around) (string)
  (when (string-directory-mode-proper-p string)
    (let ((name-index *directory-mode-name-preceding-offset*)
          (len (length string)))
      (loop (when (>= name-index len) (return))
            (let ((prev-char (schar string name-index)))
              (incf name-index)
              (when (eql prev-char #\space) (return name-index)))))))
