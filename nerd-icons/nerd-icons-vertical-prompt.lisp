;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

(in-package vprompt)

(defadvice (file-candidate-function nerd-icons :around) (&optional allow-directory only-directory)
  (lambda (input)
    (let* ((path  (parse-namestring input))
           (files (directory (make-pathname :name :wild :type :wild :defaults path)))
           result)
      (when (null allow-directory)
        (setq files (delete-if #'file-directory-p files)))
      (when only-directory
        (setq files (delete-if-not #'file-directory-p files)))
      (if (or (zerop (length input))
              (and (member (pathname-name path) '(nil :unspecific))
                   (member (pathname-type path) '(nil :unspecific))))
        (setq result (sort files (lambda (f1 f2) (string< (namestring f1) (namestring f2)))))
        (progn
          (dolist (file files)
            (let ((target (or (file-namestring path) (car (last (pathname-directory path)))))
                  (source (or (file-namestring file) (car (last (pathname-directory file))))))
              (when-let (starts (fuzzy-search target source))
                (push (list file starts source) result))))
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
                            (< (length string1) (length string2))
                            (< n1 n2))))))
                :key #'cdr)
          (setq result (mapcar #'first result))))
      (let* ((pane (window-text-pane (editor::current-echo-area-window)))
             (font (gp:find-best-font pane
                                      (gp:make-font-description
                                       :family nerd-icons:*nerd-font-family*
                                       :size (gp:font-description-attribute-value
                                              (gp:font-description (capi:simple-pane-font pane))
                                              :size)))))
        (mapcar (lambda (path)
                  (setq path (truename path))
                  (multiple-value-bind (icon color) (nerd-icons:file-to-icon-char path)
                    (let* ((name (or (file-namestring path)
                                     (string-append (car (last (pathname-directory path))) "/")))
                           (icon-str (editor::make-buffer-string
                                      :%string (string icon)
                                      :properties `((0 1 (editor:face ,(make-face nil :font font :foreground color))))))
                           (name-str (editor::make-buffer-string
                                      :%string (string-append " " name)
                                      :properties `((0 ,(1+ (length name)) (editor:face editor::default))))))
                      (make-candidate
                       :string (namestring path)
                       :display-string (editor::concatenate-buffer-strings icon-str name-str)))))
                result)))))

(defadvice (buffer-candidate-function nerd-icons :around) (buffers)
  (candidate-func-from-candidates
   (let* ((pane (window-text-pane (editor::current-echo-area-window)))
          (font (gp:find-best-font pane
                                   (gp:make-font-description
                                    :family nerd-icons:*nerd-font-family*
                                    :size (gp:font-description-attribute-value
                                           (gp:font-description (capi:simple-pane-font pane))
                                           :size)))))
     (mapcar (lambda (buf)
               (multiple-value-bind (icon color)
                   (if-let (path (buffer-pathname buf))
                       (nerd-icons:file-to-icon-char path)
                     (get 'nerd-icons::fa-file-o :char))
                 (let* ((name (buffer-name buf))
                        (icon-str (editor::make-buffer-string
                                   :%string (string icon)
                                   :properties `((0 1 (editor:face ,(make-face nil :font font :foreground color))))))
                        (name-str (editor::make-buffer-string
                                   :%string (string-append " " name)
                                   :properties `((0 ,(1+ (length name)) (editor:face editor::default))))))
                   (make-candidate
                    :string name
                    :display-string (editor::concatenate-buffer-strings icon-str name-str)
                    :marginalia (if (buffer-pathname buf)
                                  (namestring (buffer-pathname buf))
                                  "")))))
             buffers))))
