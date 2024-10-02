;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

;; Enhancement & complement of the Directory Mode. Makes it more similar with Emacs's Dired

;; Features:
;;     Command for keys: ^, +, U, L, B, C, R, w, ~, #
;;     Complement for edge cases (like in commands C & R), Make them DWIM
;;     Bugfix

(in-package editor)

;; 01Oct24: Implement this function in our own, to get rid of any dependency
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

;; The special verify-func used by our copy / rename commands,
;; Allowing both directory or file, only check wildcards.
(defun directory-mode-move-or-copy-file-prompt-verify (string parse-inf)
  (declare (ignore parse-inf))
  (if (and (stringp string) (= (length string) 0))
      (values nil "Illegal input : empty string")
    (when-let (pn (if (pathnamep string) string
                    (pathname-or-lose (relevant-pathname-end string))))
      (if (wild-pathname-p pn)
          (progn (message "~a has a wildcard in it" pn)
            nil)
        pn))))

;; 02Oct24: Able to load ASDF system using Directory Mode Do Load.
(defun directory-mode-do-load-file (path)
  "Load file at PATH. Assume the PATH is a TRUENAME.

If given file is an ASDF system definition file, load the file and
then load each system being defined in this file."
  (if (string-equal (pathname-type path) "asd")
      (progn
        (load path :package "ASDF")
        (asdf:map-systems
         (lambda (system)
           (when (equal (asdf:system-source-file system) path)
             (asdf:load-system system))))
        t)
    (load path)))

;; Editor functions advicing
;; 01Oct24: Use advice instead of arbitrary redefinition

(defadvice (directory-mode-delete-deleted-lines-files lw-plugins :around) (buffer)
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
                 (when (confirm-it (format nil "Recursively delete ~A?" full-pathname))
                   (delete-directory-tree (truename full-pathname))
                   (incf count))
               (when (delete-file full-pathname nil)
                 (incf count)))
             :delete-current-line))))
    count))

(defadvice (directory-mode-move-or-copy-marked-lines-files lw-plugins :around)
    (buffer target-directory copy-p)
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

(defadvice (directory-mode-do-move-or-copy-files lw-plugins :around) (buffer copy-p)
  (let ((want-count (directory-mode-count-marked-lines buffer)))
    ;; The original function wrongly use PROMPT-FOR-DIRECTORY and will raise error
    ;; Fix this bug by replacing a correct one
    (when-let (target-direcory (prompt-for-directory :prompt (format nil "select target directory for ~a the ~d marked files: "
                                                                     (if copy-p "copying" "moving") want-count)))
      (let ((count (directory-mode-move-or-copy-marked-lines-files buffer target-direcory copy-p)))
        (unless (or copy-p (zerop count)) (clear-undo buffer))
        (message "~a ~d files to ~a"
                 (if copy-p "copied" "moved") count target-direcory)))))

;; Commands

(defcommand "Directory Mode Up Directory" (p)
     "Visit the parent directory using Directory Mode."
     "Visit the parent directory using Directory Mode."
  (loop repeat (or p 1)
        do (list-directory-command
            nil (truename
                 (merge-pathnames (make-pathname :directory '(:relative :up))
                                  (directory-mode-buffer-directory (current-buffer)))))))

(defcommand "Directory Mode Unmark All Marks" (p)
     "Unmark all marked file in current Directory Mode buffer."
     "Unmark all marked file in current Directory Mode buffer."
  (declare (ignore p))
  (directory-mode-command-set-all-marked nil))

;; 01Oct24: Add support for loading / compiling marked files
(defcommand "Directory Mode Do Load" (p)
     "Load the target file(s)."
     "Load the target file(s)."
  (declare (ignore p))
  (let* ((point (current-point))
         (buffer (point-buffer point))
         (dir (directory-mode-buffer-directory buffer))
         (count (directory-mode-count-marked-lines buffer)))
    (if (> count 0)
        (directory-mode-map-lines
         buffer
         (lambda (string)
           (when (string-directory-mode-marked-p string)
             (let ((path (merge-pathnames (string-directory-mode-filename string) dir)))
               (handler-case (directory-mode-do-load-file (truename path))
                 (error (e) (editor-error "Cannot load ~A: ~A" (file-namestring path) e)))))))
      (let ((path (merge-pathnames (directory-mode-point-filename point) dir)))
        (handler-case
            (when (directory-mode-do-load-file (truename path))
              (message "~A has been loaded." (file-namestring path)))
          (error (e) (editor-error "Cannot load ~A: ~A" (file-namestring path) e)))))))

;; 01Oct24: Ask FASL destination before compile; Support compile in-memory
(defcommand "Directory Mode Do Compile" (p)
     "Compile the target files. Call with prefix argument to compile it in-memory and load."
     "Compile the target files. Call with prefix argument to compile it in-memory and load."
  (let* ((point (current-point))
         (buffer (point-buffer point))
         (dir (directory-mode-buffer-directory buffer))
         (count (directory-mode-count-marked-lines buffer)))
    (if (> count 0)
        (let ((output (unless p
                        (prompt-for-file :prompt "select target directory for FASL files: "
                                         :default dir
                                         :must-exist nil
                                         :file-directory-p t))))
          (directory-mode-map-lines
           buffer
           (lambda (string)
             (when (string-directory-mode-marked-p string)
               (let* ((name (string-directory-mode-filename string))
                      (path (merge-pathnames name dir)))
                 (multiple-value-bind (out warnings-p error-p)
                     (apply #'compile-file (truename path)
                            (if p (list :in-memory t :load t)
                              (list :output-file output)))
                   (declare (ignore out warnings-p))
                   (when error-p (editor-error (format nil "Error while compile ~A."))))))))
          (if p (message "~A files have been compiled in memory and loaded." count)
            (message "~A files have been compiled to ~A." count output)))
      (let* ((name (directory-mode-point-filename point))
             (path (merge-pathnames name dir))
             (output (unless p
                       (prompt-for-file :prompt (format nil "Compile ~A to: " name)
                                        :must-exist nil
                                        :default dir
                                        :verify-func #'directory-mode-move-or-copy-file-prompt-verify))))
        (multiple-value-bind (out warnings-p error-p)
            (compile-file (truename path) :output-file output)
          (declare (ignore warnings-p))
          (if error-p (editor-error "Error while compile ~A." name)
            (if p (message "~A has been compiled in memory and loaded." name)
              (message "~A has been compiled to ~A." name out))))))))

(defcommand "Directory Mode Create Directory" (p)
     "Create a directory in current directory."
     "Create a directory in current directory."
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
     "Copy marked files' name. The names are separated by a space.

With a prefix argument P, copy next P lines files' name."
     "Copy marked files' name. The names are separated by a space.

With a prefix argument P, copy next P lines files' name."
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
     "Flag all backup files (name end up with '~') for deletion."
     "Flag all backup files (name end up with '~') for deletion."
  (declare (ignore p))
  (directory-mode-command-check-and-set-flag
   (lambda (point)
     (when-let (type (pathname-type (directory-mode-point-filename point)))
       (eql (char type (1- (length type))) #\~)))
   'point-directory-mode-set-delete
   t))

(defcommand "Directory Mode Flag Auto Save Files" (p)
     "Flag all auto-save files (name around with '#') for deletion."
     "Flag all auto-save files (nams around with '#') for deletion."
  (declare (ignore p))
  (directory-mode-command-check-and-set-flag
   (lambda (point)
     (when-let (type (pathname-type (directory-mode-point-filename point)))
       (and (eql (char type 0) #\#) (eql (char type (1- (length type))) #\#))))
   'point-directory-mode-set-delete
   t))

(defcommand "Directory Mode Flag FASL Files" (p)
     "Flag all FASL files for deletion."
     "Flag all FASL files for deletion."
  (declare (ignore p))
  (directory-mode-command-check-and-set-flag
   (lambda (point)
     (when-let (type (pathname-type (directory-mode-point-filename point)))
       (and (>= (length type) 4)
            (string-equal type "fasl" :start1 (- (length type) 4)))))
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
(bind-key "Directory Mode Flag FASL Files" #(#\% #\&) :mode "Directory")
