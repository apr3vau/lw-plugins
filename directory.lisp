;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

;; Enhancement & complement of the Directory Mode. Makes it more similar with Emacs's Dired

;; Features:
;;     Command for keys: ^, +, U, L, B, C, R, w, E, ~, #
;;     Supporting Kill-when-Opening
;;     Supporting print file size in human-readable form
;;     Complement for edge cases (like in commands C & R), Make them DWIM
;;     Bugfix

(in-package editor)

;; 07Oct24: Similar with dired-kill-when-opening-new-dired-buffer
(unless (boundp '*directory-mode-kill-when-opening-new-dired-buffer*)
  (defvar *directory-mode-kill-when-opening-new-dired-buffer* nil
    "If this option is T, kill the old Directory Mode
buffer when opening new one."))

;; 07Oct24: Allow Directory Mode to print human-readable sizes for
;; files. The behavior can be controlled by this variable.
(unless (boundp '*directory-mode-print-human-readable-size*)
  (defvar *directory-mode-print-human-readable-size* t
    "If this option is T, print the file size in
human-readable form in Directory Mode, just like `ls -h'."))

(export '(*directory-mode-kill-when-opening-new-dired-buffer*
          *directory-mode-print-human-readable-size*))

;; 01Oct24: Implement this function in our own, to get rid of any dependency
(defun delete-directory-tree (dir)
  "Recursively delete directory and its contents."
  (let* ((truename (truename dir))
         (files    (sort (directory (make-pathname :name :wild :type :wild
                                                   :directory (append (pathname-directory truename) (list :wild-inferiors))
                                                   :defaults truename)
                                    :directories t :link-transparency nil)
                         (lambda (p1 p2)
                           (> (length (pathname-directory p1))
                              (length (pathname-directory p2)))))))
    (flet ((dir-p (file) (not (or (stringp (pathname-name file))
                                  (stringp (pathname-type file))))))
      (map nil (lambda (file) (unless (dir-p file) (delete-file file))) files)
      (map nil (lambda (file) (when (dir-p file) (delete-directory file))) files))
    (handler-case (delete-directory truename t)
      (error (e) (editor-error "Cannot delete directory: ~A" truename)))))

(defun directory-pathname-p (pathname)
  (declare (inline directory-pathname-p))
  (and (member (pathname-name pathname) '(nil :unspecific))
       (member (pathname-type pathname) '(nil :unspecific))))

(defun copy-directory (target new)
  (unless (directory-pathname-p new)
    (setq new (make-pathname
               :name :unspecific :type :unspecific
               :directory (append (pathname-directory new) (list (file-namestring new))))))
  (let* ((old-dir (pathname-directory target))
         (new-dir (pathname-directory new))
         (old-dir-length (length old-dir)))
    (loop for file in (directory
                       (make-pathname :name :wild :type :wild
                                      :directory (append old-dir '(:wild-inferiors))
                                      :defaults target))
          for new-file = (make-pathname
                          :name (pathname-name file) :type (pathname-type file)
                          :directory (append new-dir (subseq (pathname-directory file) old-dir-length))
                          :defaults new)
          do (ensure-directories-exist new-file)
             (copy-file file new-file))
    new))

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

;; For human-readable size:

(defadvice (insert-directory-mode-string-size-pairs lw-plugins :around) (point string-size-pairs)
  (if *directory-mode-print-human-readable-size*
      (dotimes (index (fill-pointer string-size-pairs))
        (let ((pair (aref string-size-pairs index)))
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
            (let ((string (format nil "~7<~;~A~> ~A~%" (print-size (cdr pair)) (car pair))))
              (insert-things point *directory-mode-prefix* string)))))
    (call-next-advice point string-size-pairs)))

;; For kill when opening:

(defadvice (internal-directory-mode-edit-file lw-plugins :after) (p other-window)
  (declare (ignore p other-window))
  (when (and *directory-mode-kill-when-opening-new-dired-buffer*
             (string= (buffer-major-mode-name (current-buffer)) "Directory"))
    (dolist (buf *buffer-list*)
      (when (and (not (eq buf (current-buffer)))
                 (string= (buffer-major-mode-name buf) "Directory"))
        (kill-buffer-no-confirm buf)))))

(defadvice (directory-mode-new-buffer-with-filter lw-plugins :around) (from-buffer filter)
  (declare (ignore from-buffer filter))
  (when *directory-mode-kill-when-opening-new-dired-buffer*
    (dolist (buf *buffer-list*)
      (when (and (not (eq buf (current-buffer)))
                 (string= (buffer-major-mode-name buf) "Directory"))
        (kill-buffer-no-confirm buf)))))

(defadvice (list-directory-command lw-plugins :after) (p &optional directory)
  (declare (ignore p directory))
  (when *directory-mode-kill-when-opening-new-dired-buffer*
    (dolist (buffer *buffer-list*)
      (when (and (not (eq buffer (current-buffer)))
                 (string= (buffer-major-mode-name buffer) "Directory"))
        (kill-buffer-no-confirm buffer)))))

;; Misc

(defadvice (directory-mode-delete-deleted-lines-files lw-plugins :around) (buffer)
  (let ((directory (directory-mode-buffer-directory buffer))
        (count     0))
    (directory-mode-map-lines-modifying
     buffer
     #'(lambda (string)
         (when (string-directory-mode-deleted-p string)
           (let* ((name          (string-directory-mode-filename string))
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
        (count     0))
    (directory-mode-map-lines-modifying
     buffer
     #'(lambda (string)
         (when (string-directory-mode-marked-p string)
           (let* ((name (string-directory-mode-filename string))
                  (from (merge-pathnames name directory))
                  (to   (merge-pathnames name target-directory)))
             ;; Add file-exist check here
             (when (or (not (probe-file to))
                       (confirm-it (format nil "~A exists, replace it?" to)))
               (if copy-p
                 ;; 14Nov24: Support copy directory
                 (if (file-directory-p from)
                   (copy-directory from to)
                   (copy-file from to))
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

;; 07Oct24: Advicing instead of redefining
;; Features we add:
;;     Support bulk-rename marked files;
;;     Allow input of both new directory or new name
(defadvice (directory-mode-rename-command lw-plugins :around) (p)
  (declare (ignore p))
  (let* ((point  (current-point))
         (buffer (point-buffer point))
         (dir    (directory-mode-buffer-directory buffer)))
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
            (if (directory-pathname-p new)
              (progn
                (if (probe-file new)
                  (when (file-directory-p old)
                    (if (confirm-it (format nil "Directory ~A exists.  Overwrite it anyway?" new))
                      (delete-directory-tree new)
                      (return)))
                  (unless (file-directory-p old)
                    (if (confirm-it (format nil "Directory ~A does not exist.  Create it?" new))
                      (ensure-directories-exist new)
                      (return))))
                (unless (file-directory-p old)
                  (setf new (merge-pathnames (file-namestring old) new))))
              (if (probe-file new)
                (unless (confirm-it (format nil "File ~A exists.  Overwrite it anyway?" new))
                  (return))))
            (when (rename-file old new)
              (revert-buffer-command nil)
              (message "renamed ~a to ~a" name new))))))))

;; Features we add:
;;     Support copy only current-line's file when nothing marked
;;     Allow input of both new directory or new name
(defadvice (directory-mode-copy-marked-command lw-plugins :around) (p)
  (declare (ignore p))
  (let* ((point  (current-point))
         (buffer (point-buffer point))
         (dir    (directory-mode-buffer-directory buffer)))
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
            (if (directory-pathname-p new)
              (progn
                (if (probe-file new)
                  (when (file-directory-p old)
                    (if (confirm-it (format nil "Directory ~A exists.  Overwrite it anyway?" new))
                      (delete-directory-tree new)
                      (return)))
                  (unless (file-directory-p old)
                    (if (confirm-it (format nil "Directory ~A does not exist.  Create it?" new))
                      (ensure-directories-exist new)
                      (return))))
                (unless (file-directory-p old)
                  (setf new (merge-pathnames (file-namestring old) new))))
              (if (probe-file new)
                (unless (confirm-it (format nil "File ~A exists.  Overwrite it anyway?" new))
                  (return))))
            (when (if (file-directory-p old)
                    (copy-directory old new)
                    (copy-file old new))
              (revert-buffer-command nil)
              (message "Copied ~a to ~a" name new))))))))

;; New Commands

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
  (let* ((point  (current-point))
         (buffer (point-buffer point))
         (dir    (directory-mode-buffer-directory buffer))
         (count  (directory-mode-count-marked-lines buffer)))
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
  (let* ((point  (current-point))
         (buffer (point-buffer point))
         (dir    (directory-mode-buffer-directory buffer))
         (count  (directory-mode-count-marked-lines buffer)))
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
      (let* ((name   (directory-mode-point-filename point))
             (path   (merge-pathnames name dir))
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
         (if (or (not (member (pathname-name path) '(nil :unspecific)))
                 (not (member (pathname-type path) '(nil :unspecific))))
           (merge-pathnames (make-pathname :directory (list :relative (file-namestring path)))
                            (pathname-location path))
           path))
        (revert-buffer-command nil)))))

(defcommand "Directory Mode Copy Filename" (p)
     "Copy marked files' name. The names are separated by a space.

With a prefix argument P, copy next P lines files' name."
     "Copy marked files' name. The names are separated by a space.

With a prefix argument P, copy next P lines files' name."
  (setq p (or p 1))
  (let* ((point  (current-point))
         (buffer (point-buffer point))
         paths)
    (directory-mode-map-lines
     buffer
     #'(lambda (string)
         (when (string-directory-mode-marked-p string)
           (push-end (string-directory-mode-filename string) paths))))
    (let ((str (format nil "~{~A~^ ~}"
                       (or paths
                           (loop repeat p
                                 for pt = (copy-point point :temp) then (line-offset pt 1)
                                 until (null pt)
                                 collect (directory-mode-point-filename pt))))))
      (save-kill-text nil str)
      (message "~A" str))))

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
     "Flag all auto-save files (name around with '#') for deletion."
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

(defcommand "Directory Mode Do Open" (p)
     "Open the marked or next P files using external program."
     "Open the marked or next P files using external program."
  (setq p (or p 1))
  (let* ((point  (current-point))
         (buffer (point-buffer point))
         (dir    (directory-mode-buffer-directory buffer))
         thereis-marked-p)
    (labels ((find-executable (name)
               (loop for dir in (split-sequence '(#\:) (environment-variable "PATH"))
                     thereis (car (directory (make-pathname :name name :defaults (truename dir))))))
             (open-file (path)
               (sys:call-system (string-append #+mswindows "start"
                                               #+darwin "open"
                                               #+linux (or (find-executable "xdg-open") "open")
                                               " " (namestring (truename path))))))
      (directory-mode-map-lines
       buffer
       (lambda (string)
         (when (string-directory-mode-marked-p string)
           (open-file (merge-pathnames (string-directory-mode-filename string) dir))
           (setq thereis-marked-p t))))
      (unless thereis-marked-p
        (loop repeat p
              for pt = (copy-point point :temp) then (line-offset pt 1)
              until (null pt)
              do (open-file (merge-pathnames (directory-mode-point-filename pt) dir)))))))

;; Bindings

(bind-key "Bury Buffer"                         "q" :mode "Directory")
(bind-key "Directory Mode Up Directory"         "^" :mode "Directory")
(bind-key "Directory Mode Unmark All Marks"     "U" :mode "Directory")
(bind-key "Directory Mode Do Load"              "L" :mode "Directory")
(bind-key "Directory Mode Do Compile"           "B" :mode "Directory")
(bind-key "Directory Mode Do Open"              "E" :mode "Directory")
(bind-key "Directory Mode Create Directory"     "+" :mode "Directory")
(bind-key "Directory Mode Rename"               "R" :mode "Directory")
(bind-key "Directory Mode Copy Marked"          "C" :mode "Directory")
(bind-key "Directory Mode Copy Filename"        "w" :mode "Directory")
(bind-key "Directory Mode Flag Backup Files"    "~" :mode "Directory")
(bind-key "Directory Mode Flag Auto Save Files" "#" :mode "Directory")
;; My personal preference here :D
(bind-key "Directory Mode Flag FASL Files"      #(#\% #\&) :mode "Directory")
