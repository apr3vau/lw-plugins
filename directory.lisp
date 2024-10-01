;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

;; Enhancement & complement of the Directory Mode. Makes it more similar with Emacs's Dired

;; Features:
;;     Command for keys: ^, +, U, L, B, C, R, w, ~, #
;;     Complement for edge cases (like in commands C & R)
;;     Some bugfix

(in-package editor)

(defun delete-directory-tree (dir)
  "Recursively delete directory and its contents."
  #+lispworks7+
  (fast-directory-files
   dir
   (lambda (name handle)
     (let ((fullname (string-append (fdf-handle-directory-string handle)
                                    name)))
       (if (fdf-handle-directory-p handle)
           (delete-directory-tree fullname)
         (handler-case (delete-file fullname t)
           (error (e) (editor-error "Cannot delete file: ~A" fullname)))))))
  #-lispworks7+
  (dolist (file (directory (make-pathname :name :wild :type :wild
                                          :defaults (truename dir))))
    (if (file-directory-p file)
        (delete-directory-tree file)
      (handler-case (delete-file fullname t)
        (error (e) (editor-error "Cannot delete file: ~A" fullname)))))
  
  (handler-case (delete-directory dir t)
    (error (e) (editor-error "Cannot delete directory: ~A" dir))))

(defun directory-mode-delete-deleted-lines-files (buffer)
  (let ((directory (directory-mode-buffer-directory buffer))
        (count 0))
    (directory-mode-map-lines-modifying
     buffer
     #'(lambda (string)
         (when (string-directory-mode-deleted-p string)
           (let* ((name (string-directory-mode-filename string))
                  (full-pathname (merge-pathnames name directory)))
             (if (file-directory-p full-pathname)
                 ;; Makes it able to delete directory
                 (when (confirm-it (format nil "Delete directory ~A and its content?" full-pathname))
                   (delete-directory-tree (truename full-pathname))
                   (incf count))
               (when (delete-file full-pathname nil)
                 (incf count)))
             :delete-current-line))))
    count))

(defun directory-mode-move-or-copy-marked-lines-files (buffer target-directory copy-p)
  (let ((directory (directory-mode-buffer-directory buffer))
        (count 0))
    (directory-mode-map-lines-modifying
     buffer
     #'(lambda (string)
         (when (string-directory-mode-marked-p string)
           (let* ((name (string-directory-mode-filename string))
                  (from (merge-pathnames name directory))
                  (to (merge-pathnames name target-directory)))
             ;; Add file-exist check here
             (when (or (not (probe-file to))
                       (confirm-it (format nil "~A exists, replace it?" to)))
               (if copy-p
                   (copy-file from to)
                 (rename-file from to))
               (incf count)
               (unless copy-p
                 :delete-current-line))))))
    count))

(defun directory-mode-do-move-or-copy-files (buffer copy-p)
  (let ((want-count (directory-mode-count-marked-lines buffer)))
    ;; The original function wrongly use PROMPT-FOR-DIRECTORY and will raise error
    ;; Fix this bug by replacing a correct one
    (when-let (target-direcory (prompt-for-directory :prompt (format nil "select target directory for ~a the ~d marked files: "
                                                                     (if copy-p "copying" "moving") want-count)))
      (let ((count (directory-mode-move-or-copy-marked-lines-files buffer target-direcory copy-p)))
        (unless (or copy-p (zerop count)) (clear-undo buffer))
        (message "~a ~d files to ~a"
                 (if copy-p "copied" "moved") count target-direcory)))))

;; The special verify-func used by our own copy / rename commands,
;; Accept both directory or file.
(defun directory-mode-move-or-copy-file-prompt-verify (string parse-inf)
  (declare (type parse-inf parse-inf))
  (when (and (stringp string) (= (length string) 0))
    (let ((me (parse-inf-must-exist parse-inf)))
      (unless (eq me t)
        (return-from directory-mode-move-or-copy-file-prompt-verify
          (values nil (if me "Illegal input : empty string" t))))))
  (let ((pn (if (pathnamep string)
                string
              (or (pathname-or-lose (relevant-pathname-end string))
                  (let ((sys::*twiddle-active* nil))
                    (pathname-or-lose (relevant-pathname-end string)))))))
    (when pn
      (cond ((wild-pathname-p pn)
             (message "~a has a wildcard in it" pn)
             nil)
	    ((not (eq (parse-inf-must-exist parse-inf) t)) pn)
	    (t nil)))))

(defcommand "Directory Mode Up Directory" (p)
     "" ""
  (loop repeat (or p 1)
        do (list-directory-command
            nil (truename
                 (merge-pathnames (make-pathname :directory '(:relative :up))
                                  (directory-mode-buffer-directory (current-buffer)))))))

(defcommand "Directory Mode Unmark All Marks" (p)
     "" ""
  (declare (ignore p))
  (directory-mode-command-set-all-marked nil))

(defcommand "Directory Mode Do Load" (p)
     "" ""
  (declare (ignore p))
  (let ((path (merge-pathnames (directory-mode-point-filename (current-point))
                              (directory-mode-buffer-directory (current-buffer)))))
    (handler-case
        (when (load (truename path))
          (message "~A has been loaded." (file-namestring path)))
      (error (e) (editor-error e)))
    ))

(defcommand "Directory Mode Do Compile" (p)
     "" ""
  (declare (ignore p))
  (let ((path (merge-pathnames (directory-mode-point-filename (current-point))
                               (directory-mode-buffer-directory (current-buffer)))))
    (handler-case
        (message "~A has been compiled to ~A" (file-namestring path)
                 (file-namestring (compile-file (truename path) :load t)))
      (error (e) (editor-error e)))))

(defcommand "Directory Mode Create Directory" (p)
     "" ""
  (declare (ignore p))
  (let ((path (prompt-for-file :prompt "Create Directory: "
                               :default (directory-mode-buffer-directory (current-buffer))
                               :must-exist nil)))
    (if (probe-file path)
        (editor-error (format nil "Cannot create directory ~A: file exists"))
      (progn
        (ensure-directories-exist
         (if (pathname-name path)
             (merge-pathnames (make-pathname :directory (list :relative (pathname-name path)))
                              (pathname-location path))
           path))
        (revert-buffer-command nil)))))

(defcommand "Directory Mode Copy Filename" (p)
     "" ""
  (let* ((point (current-point))
         (buffer (point-buffer point))
         (dir (directory-mode-buffer-directory buffer)))
    (if p
        (with-point ((temp point))
          (save-kill-text nil (loop repeat p
                                    for name = (directory-mode-point-filename point)
                                    collect (namestring (merge-pathnames name dir)))))
      (let (paths)
        (directory-mode-map-lines
         buffer
         #'(lambda (string)
             (when (string-directory-mode-marked-p string)
               (push-end (namestring
                          (merge-pathnames (string-directory-mode-filename string) dir))
                         paths))))
        (when paths (save-kill-text nil (format nil "~{~A ~}" paths))
          (save-kill-text nil (namestring (merge-pathnames (directory-mode-point-filename point) dir))))))))

(defcommand "Directory Mode Flag Backup Files" (p)
     "" ""
  (declare (ignore p))
  (directory-mode-command-check-and-set-flag
   (lambda (point)
     (when-let (type (pathname-type (directory-mode-point-filename point)))
       (eql (char type (1- (length type))) #\~)))
   'point-directory-mode-set-delete
   t))

(defcommand "Directory Mode Flag Auto Save Files" (p)
     "" ""
  (declare (ignore p))
  (directory-mode-command-check-and-set-flag
   (lambda (point)
     (when-let (type (pathname-type (directory-mode-point-filename point)))
       (and (eql (char type 0) #\#) (eql (char type (1- (length type))) #\#))))
   'point-directory-mode-set-delete
   t))

(defcommand "Directory Mode Flag FASL Files" (p)
     "" ""
  (declare (ignore p))
  (directory-mode-command-check-and-set-flag
   (lambda (point)
     (when-let (type (pathname-type (directory-mode-point-filename point)))
       (uiop:string-suffix-p type "fasl")))
   'point-directory-mode-set-delete
   t))

;; Features we add:
;;     Support bulk-rename marked files;
;;     Allow input of both new directory or new name
(defcommand "Directory Mode Rename" (p)
     "Prompt for a new name and rename the file in the current line."
     "Prompt for a new name and rename the file in the current line."
  (declare (ignore p))
  (let* ((point (current-point))
         (buffer (point-buffer point))
         (dir (directory-mode-buffer-directory buffer)))
    (if (> (directory-mode-count-marked-lines buffer) 0)
        (directory-mode-do-move-or-copy-files buffer nil)
      (when-let* ((name (directory-mode-point-filename point))
                  (new (prompt-for-file :default dir
                                        :directory :output
                                        :must-exist nil
                                        :prompt (format nil "Rename ~a to: " name)
                                        :verify-func #'directory-mode-move-or-copy-file-prompt-verify)))
        (block nil
          (let ((old (merge-pathnames name dir)))
            (unless (or (pathname-name new) (pathname-type new))
              (if (probe-file new)
                  (when (file-directory-p old)
                    (if (prompt-for-y-or-n :prompt (format nil "Directory ~A exists.  Overwrite it anyway?" new))
                        (delete-directory-tree new)
                      (return)))
                (unless (file-directory-p old)
                  (if (prompt-for-y-or-n :prompt (format nil "Directory ~A does not exist.  Create it?" new))
                      (ensure-directories-exist new)
                    (return))))
              (unless (file-directory-p old)
                (setf new (merge-pathnames (file-namestring old) new))))
            (when (rename-file old new)
              (revert-buffer-command nil)
              (message "renamed ~a to ~a" name new))))))))

;; Features we add:
;;     Support copy only current-line's file when nothing marked
;;     Allow input of both new directory or new name
(defcommand "Directory Mode Copy Marked" (p)
     "Copy the files that are marked to another directory"
     "Copy the files that are marked to another directory"
  (declare (ignore p))
  (let* ((point (current-point))
         (buffer (point-buffer point))
         (dir (directory-mode-buffer-directory buffer)))
    (if (> (directory-mode-count-marked-lines buffer) 0)
        (directory-mode-do-move-or-copy-files buffer t)
      (when-let* ((name (directory-mode-point-filename point))
                  (new (prompt-for-file :default dir
                                        :directory :output
                                        :must-exist nil
                                        :prompt (format nil "Copy ~a to: " name)
                                        :verify-func #'directory-mode-move-or-copy-file-prompt-verify)))
        (block nil
          (let ((old (merge-pathnames name dir)))
            (unless (or (pathname-name new) (pathname-type new))
              (if (probe-file new)
                  (when (file-directory-p old)
                    (if (prompt-for-y-or-n :prompt (format nil "Directory ~A exists.  Overwrite it anyway?" new))
                        (delete-directory-tree new)
                      (return)))
                (unless (file-directory-p old)
                  (if (prompt-for-y-or-n :prompt (format nil "Directory ~A does not exist.  Create it?" new))
                      (ensure-directories-exist new)
                    (return))))
              (unless (file-directory-p old)
                (setf new (merge-pathnames (file-namestring old) new))))
            (when (copy-file old new)
              (revert-buffer-command nil)
              (message "Copied ~a to ~a" name new))))))))

(bind-key "Directory Mode Up Directory" "^" :mode "Directory")
(bind-key "Directory Mode Unmark All Marks" "U" :mode "Directory")
(bind-key "Directory Mode Do Load" "L" :mode "Directory")
(bind-key "Directory Mode Do Compile" "B" :mode "Directory")
(bind-key "Directory Mode Create Directory" "+" :mode "Directory")
(bind-key "Directory Mode Rename" "R" :mode "Directory")
(bind-key "Directory Mode Copy Marked" "C" :mode "Directory")
(bind-key "Directory Mode Copy Filename" "w" :mode "Directory")
(bind-key "Directory Mode Flag Backup Files" "~" :mode "Directory")
(bind-key "Directory Mode Flag Auto Save Files" "#" :mode "Directory")
;; My personal preference here :D
(bind-key "Directory Mode Flag FASL Files" "." :mode "Directory")