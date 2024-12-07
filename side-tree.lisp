;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

;; This is the Side Tree, a LispWorks project explorer alternative to
;; Treemacs and NerdTree. If you're familiar with Treemacs, you can
;; start to use it seamlessly.

;; We're aimed to replicate all useful features in Treemacs, plus some
;; Lisp oriented functions. The default key bindings are designed for
;; compatible with both Treemacs and Dired (Directory Mode).

;; Here're features we support:
;; - Workspaces and Projects;
;; - Sub-menu for Lisp definitions
;; - File actions (create, rename, delete) for both file and directory;
;; - Custom icons
;; ...

;; Usage: Load this file, press M-0 to open the Side Tree.

(defpackage side-tree
  (:use :editor)
  (:add-use-defaults))
(in-package side-tree)

(defvar *config-file*
  (ensure-directories-exist
   (merge-pathnames "lw-plugins/side-tree.lisp" (sys:get-folder-path :appdata)))
  "The config file to persistant workspaces and projects of Side Tree")

(defvar *default-width* 40
  "Default width of Side Tree window.")
(export '*default-width*)

(defvar *workspaces*)
(defvar *windows-plist* nil
  "A plist of editor pane -> side tree window.")

;; Used for Lisp imenu
(defvar *lisp-functions-symbols*
  '(defun defadvice))
(defvar *lisp-methods-symbols*
  '(defgeneric defmethod))
(defvar *lisp-variables-symbols*
  '(defvar defparameter defconstant define-editor-variable fli:define-foreign-variable))
(defvar *lisp-types-symbols*
  '(deftype defstruct defclass defpackage defmode define-condition))
(defvar *lisp-macros-symbols*
  '(defmacro define-symbol-macro define-compiler-macro define-modify-macro
     defsetf define-setf-expander))
(defvar *capi-symbols*
  '(capi:define-interface capi:define-layout capi:define-menu capi:define-command))
(defvar *editor-commands-symbols*
  '(defcommand))
(defvar *styles-symbols*
  '(color:define-color-alias make-face))

;; Types

(defstruct workspace name (projects nil))
(export '(workspace workspace-name workspace-projects name projects
                    make-workspace copy-workspace workspace-p))

(defstruct project name path)
(export '(project project-name project-path path
                  make-project copy-project project-p))

(define-editor-variable workspace nil)

;; Faces

(make-face 'project-name-face
           :if-exists :overwrite
           :foreground (editor::create-dark-background-switchable-color :forestgreen :burlywood2)
           :bold-p t)
(make-face 'directory-name-face
           :if-exists :overwrite
           :foreground (editor::create-dark-background-switchable-color :blue :lightblue))
(make-face 'file-name-face :if-exists :overwrite)
(make-face 'tag-name-face
           :if-exists :overwrite
           :foreground (editor::create-dark-background-switchable-color :orchid :pink))

(export '(project-name-face directory-name-face file-name-face tag-name-face))

;; Helper functions

;; Copied from Editor source code
(defmacro with-buffer-writable (buffer &body body)
  (rebinding (buffer)
    (with-unique-names (writable)
      `(let ((,writable (buffer-writable ,buffer)))
         (unwind-protect
             (progn (setf (buffer-writable ,buffer) t)
               ,@body)
           (setf (buffer-writable ,buffer) ,writable))))))

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

(defun prompt-for-project (workspace &rest args &key prompt)
  "Prompt for a Side Tree project in WORKSPACE by its name."
  (let ((projects (workspace-projects workspace)))
    (apply #'prompt-for-string
           :prompt prompt
           :default (if-let (project (get-text-property (current-point) :project))
                        (project-name project)
                      "")
           :complete-func (lambda (string parse-inf)
                            (declare (ignore parse-inf))
                            (sys::complete-string string (mapcar #'project-name projects)
                                                  :ignore-case t))
           :verify-func (lambda (string parse-inf)
                          (declare (ignore parse-inf))
                          (find string projects :key #'project-name :test #'equal))
           args)))

(defun prompt-for-workspace (&rest args &key prompt)
  "Prompt for a Side Tree workspace by its name."
  (apply #'prompt-for-string
         :prompt prompt
         :default (if-let (workspace (buffer-value (current-buffer) 'workspace))
                      (workspace-name workspace)
                    "")
         :complete-func (lambda (string parse-inf)
                          (declare (ignore parse-inf))
                          (sys::complete-string string (mapcar #'workspace-name *workspaces*)
                                                :ignore-case t))
         :verify-func (lambda (string parse-inf)
                        (declare (ignore parse-inf))
                        (find string *workspaces* :key #'workspace-name :test #'equal))
         args))

(defun valid-entry-p (point)
  "Check if the line at POINT is an valid Side Tree line.
i.e. The line is a Side Tree entry an has property information."
  (declare (inline valid-entry-p))
  (get-text-property point :kind))

(defun valid-entry-offset (point n)
  "Change POINT to N \"valid\" Side Tree lines offset.
Will skip those lines that not represent an entry."
  (unless (or (zerop n) (null point))
    (let ((unit (if (plusp n) 1 -1)))
      (loop do (setq point (line-offset point unit))
               (when (and point (valid-entry-p point))
                 (decf n unit))
         until (or (null point)
                   (zerop n))
         finally (return point)))))

(defun entry-expanded-p (point)
  "Check if the line at POINT is an expanded entry."
  (declare (inline entry-expanded-p))
  (get-text-property point :expanded-p))

;; Data persistance

(defun initialize ()
  "Initialize Side Tree, load the config file."
  (if (probe-file *config-file*)
    (loop for (sym val) on (with-open-file (in *config-file*)
                             (read in))
            by #'cddr
          do (set sym val))
    (setq *workspaces* (list (make-workspace :name "Default")))))

(defun save-config ()
  "Write workspaces & projects information into *CONFIG-FILE*"
  (with-open-file (out *config-file*
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (let ((*package* nil))
      (prin1 (list '*workspaces* *workspaces*) out))))

;; Generic functions

(defgeneric file-ignore-p (truename)
  (:documentation "Given a truename, determine if this file should be
ignored (not displayed) in Side Tree.")
  (:method (truename)
   (let ((filename (if (file-directory-p truename)
                     (car (last (pathname-directory truename)))
                     (file-namestring truename))))
     (or (eql (char filename (1- (length filename))) #\Tilde)
         (string= ".#" filename :end2 2)
         (and (eql (char filename 0) #\#)
              (eql (char filename (1- (length filename))) #\#))
         (member filename '(".git" ".github"
                            ".DS_Store" ".localized" "Thumbs.db"
                            "$RECYCLE.BIN")
                 :test #'equal)))))

(defgeneric get-icon (kind path)
  (:documentation "Get an icon for Side Tree.

This function should return two values, the first value is an icon
character, the second value is a plist of text properties that should
be put on the character.

Possible values of KIND are:
- :RIGHT-POINTER and :DOWN-POINTER
- :PROJECT
- :DIRECTORY and :DIRECTORY-OPENED
- :FILE
- Other values that may appeared as the KIND property of the entry.

PATH is the truename of the entry.")
  (:method (kind path))
  (:method ((kind (eql :right-pointer)) path)
   (values (code-char 9654) nil))
  (:method ((kind (eql :down-pointer)) path)
   (values (code-char 9660) nil))
  (:method ((kind (eql :project)) path)
   (values (code-char 128218) nil))
  (:method ((kind (eql :directory)) path)
   (values (code-char 128193) nil))
  (:method ((kind (eql :directory-opened)) path)
   (values (code-char 128194) nil))
  (:method ((kind (eql :file)) path)
   (if (member (pathname-type path) '("lisp" "lsp" "lispworks")
               :test #'equal)
     (values (code-char 128221) nil)
     (values (code-char 128196) nil)))
  )

(defun switch-expand-state (point)
  (let* ((pos         (point-position point))
         (truename    (get-text-property point :truename))
         (expanded-p  (not (get-text-property point :expanded-p)))
         (old-pointer (if expanded-p
                        (get-icon :right-pointer truename)
                        (get-icon :down-pointer truename))))
    (multiple-value-bind (new-pointer new-props)
        (if expanded-p
          (get-icon :down-pointer truename)
          (get-icon :right-pointer truename))
      (with-point ((point point)
                   (end point))
        (line-start point)
        (line-offset end 1 0)
        (put-text-property point end :expanded-p expanded-p :modification nil)
        (loop when (editor:point= point end)
                do (return-from switch-expand-state)
              until (eql (character-at point 0) old-pointer)
              do (editor::point-after point))
        (let ((props (text-properties-at point)))
          (loop for (key val) on new-props by #'cddr
                do (setf (getf props key) val))
          (editor::delete-characters point 1)
          (editor::insert-buffer-string
           point (editor::make-buffer-string :%string (string new-pointer) :properties `((0 1 ,props))))))
      (setf (point-position point) pos))))

(defun expand-dir (point)
  (switch-expand-state point)
  (let* ((pos      (point-position point))
         (props    (text-properties-at point))
         (level    (getf props :level))
         (project  (getf props :project))
         (truename (getf props :truename)))
    (with-point ((point point))
      (line-offset point 1 0)
      (insert-directory-content point (1+ level) project truename))
    (setf (point-position point) pos)))

(defgeneric expand (point kind)
  (:documentation "Expand the entry at POINT.
KIND is the KIND property of the entry.")
  (:method (point (kind t)))
  (:method (point (kind (eql :project)))
   (expand-dir point))
  (:method (point (kind (eql :directory)))
   (expand-dir point))
  (:method (point (kind (eql :file)))
   (let ((*read-eval* nil)
         (truename    (get-text-property point :truename))
         (level       (get-text-property point :level))
         (project     (get-text-property point :project)))
     (when (member (pathname-type truename) '("lisp" "lsp" "l" "lispworks") :test #'equal)
       (switch-expand-state point)
       (with-point ((point point))
         (line-offset point 1 0)
         (with-open-file (in truename)
           (handler-case
               (loop for i from 1
                     for form = (read in nil 'eof)
                     for car = (when (consp form) (car form))
                     until (eq form 'eof)
                     when car
                       if (member car *lisp-functions-symbols*)
                         collect (list i (second form)) into functions
                       else if (member car *lisp-methods-symbols*)
                              collect (list i (second form)) into methods
                         else if (member car *lisp-variables-symbols*)
                                collect (list i (second form)) into variables
                              else if (member car *lisp-types-symbols*)
                                     collect (list i (second form)) into types
                                else if (member car *lisp-macros-symbols*)
                                       collect (list i (second form)) into macros
                                     else if (member car *capi-symbols*)
                                            collect (list i (second form)) into capi
                                       else if (member car *editor-commands-symbols*)
                                              collect (list i (second form)) into editor-commands
                                            else if (member car *styles-symbols*)
                                                   collect (list i (second form)) into styles
                     finally
                       (flet ((insert (name forms)
                                (declare (inline insert))
                                (editor::insert-buffer-string
                                 point (make-entry-string
                                        name 'tag-name-face t
                                        :kind :lisp-imenu
                                        :truename truename :level (1+ level) :project project :expanded-p nil :forms forms))))
                         (when functions       (insert "Functions"       functions))
                         (when methods         (insert "Methods"         methods))
                         (when variables       (insert "Variables"       variables))
                         (when types           (insert "Types"           types))
                         (when macros          (insert "Macros"          macros))
                         (when capi            (insert "CAPI"            capi))
                         (when editor-commands (insert "Editor Commands" editor-commands))
                         (when styles          (insert "Styles"          styles))))
             (error (e))))))))
  (:method (point (kind (eql :lisp-imenu)))
   (switch-expand-state point)
   (let* ((props    (text-properties-at point))
          (level    (getf props :level))
          (project  (getf props :project))
          (truename (getf props :truename))
          (forms    (getf props :forms)))
     (with-point ((point point)
                  (end point))
       (line-offset point 1 0)
       (loop for (index form) in forms
             for name = (if (consp form) (car form) form)
             do (editor::insert-buffer-string
                 point (make-entry-string
                        (string-downcase name) 'tag-name-face nil
                        :kind :lisp-definition
                        :truename truename :level (1+ level) :project project :index index)))))))

(defgeneric collapse (point kind)
  (:documentation "Collapse the entry at POINT.
KIND is the KIND property of the entry.")
  (:method (point (kind t))
   (when (valid-entry-p point)
     (switch-expand-state point)
     (with-point ((point point)
                  (start point))
       (let ((level (get-text-property point :level)))
         (line-start start)
         (line-offset point 1 0)
         (put-text-property start point :expanded-p nil :modification nil)
         (loop while (and (get-text-property point :level)
                          (> (get-text-property point :level) level))
               do (with-point ((end point))
                    (line-offset end 1 0)
                    (delete-between-points point end))))))))

(defgeneric toggle (point kind)
  (:method (point kind)
   (if (entry-expanded-p point)
     (collapse point kind)
     (expand point kind)))
  (:method (point (kind (eql :lisp-definition)))
   (activate point kind)))

(defgeneric activate (point kind)
  (:documentation "Activate the entry at POINT.
KIND is the KIND property of the entry.")
  (:method (point (kind t)))
  (:method (point (kind (eql :project)))
   (if (entry-expanded-p point)
     (collapse point :project)
     (expand point :project)))
  (:method (point (kind (eql :directory)))
   (expand point :directory))
  (:method (point (kind (eql :directory-opened)))
   (collapse point :directory))
  (:method (point (kind (eql :file)))
   (let ((buffer (find-file-buffer (get-text-property point :truename)))
         (editor::*windows-not-to-use* (list (current-window))))
     (goto-buffer buffer nil nil nil)))
  (:method (point (kind (eql :lisp-imenu)))
   (if (entry-expanded-p point)
     (collapse point :project)
     (expand point :project)))
  (:method (point (kind (eql :lisp-definition)))
   (let* ((index     (get-text-property point :index))
          (buffer    (find-file-buffer (get-text-property point :truename)))
          (new-point (buffer-point buffer))
          (editor::*windows-not-to-use* (list (current-window))))
     (buffer-start new-point)
     (form-offset new-point index)
     (set-current-mark new-point)
     (form-offset new-point -1)
     (editor::set-highlight-buffer-region t buffer)
     (goto-buffer buffer nil nil 10))))

(export '(file-ignore-p get-icon expand collapse toggle activate))

;; Formatting

(defun make-entry-string (name face has-pointer-p &rest keys &key kind truename level expanded-p &allow-other-keys)
  (let ((space-len (* 2 level)))
    (multiple-value-bind (pointer pointer-props)
        (get-icon (if expanded-p :down-pointer :right-pointer) truename)
      (multiple-value-bind (icon icon-props)
          (get-icon kind truename)
        (if icon
          (if has-pointer-p
            (editor::make-buffer-string
             :%string (format nil "~A~C ~C ~A~%" (make-string space-len :initial-element #\Space) pointer icon name)
             :properties (list (list 0 space-len keys)
                               (list space-len (1+ space-len) (append keys pointer-props))
                               (list (1+ space-len) (+ space-len 2) keys)
                               (list (+ space-len 2) (+ space-len 3)
                                     (append keys icon-props))
                               (list (+ space-len 3) (+ space-len 4) keys)
                               (list (+ space-len 4) (+ space-len 5 (length name))
                                     (append keys (when face (list 'face face))))))
            (editor::make-buffer-string
             :%string (format nil "~A ~C ~A~%" (make-string space-len :initial-element #\Space) icon name)
             :properties (list (list 0 space-len keys)
                               (list space-len (1+ space-len) keys)
                               (list (1+ space-len) (+ space-len 2)
                                     (append keys icon-props))
                               (list (+ space-len 2) (+ space-len 3) keys)
                               (list (+ space-len 3) (+ space-len 4 (length name))
                                     (append keys (when face (list 'face face)))))))
          (if has-pointer-p
            (editor::make-buffer-string
             :%string (format nil "~A~C ~A~%" (make-string space-len :initial-element #\Space) pointer name)
             :properties (list (list 0 space-len keys)
                               (list space-len (1+ space-len) (append keys pointer-props))
                               (list (1+ space-len) (+ space-len 2) keys)
                               (list (+ space-len 2) (+ space-len 3 (length name))
                                     (append keys (when face (list 'face face))))))
            (editor::make-buffer-string
             :%string (format nil "~A ~A~%" (make-string space-len :initial-element #\Space) name)
             :properties (list (list 0 space-len keys)
                               (list space-len (1+ space-len) keys)
                               (list (1+ space-len) (+ space-len 2 (length name))
                                     (append keys (when face (list 'face face))))))))))))

(defun make-file-line (level project truename &optional expanded-p)
  (let* ((dir-p     (file-directory-p truename))
         (filename  (if dir-p (string-append (car (last (pathname-directory truename))) "/")
                      (file-namestring truename))))
    (make-entry-string filename (if dir-p 'directory-name-face 'file-name-face) dir-p
                       :kind (if dir-p (if expanded-p :directory-opened :directory) :file)
                       :truename truename :level level :project project :expanded-p expanded-p)))

(defun insert-directory-content (point level project directory)
  (dolist (path (sort
                 (directory (make-pathname :name :wild :type :wild :defaults directory))
                 (lambda (path1 path2)
                   (cond ((file-directory-p path1)
                          (if (file-directory-p path2)
                            (string-lessp (car (last (pathname-directory path1)))
                                          (car (last (pathname-directory path2))))
                            t))
                         ((file-directory-p path2) nil)
                         (t (string-lessp (file-namestring path1)
                                          (file-namestring path2)))))))
    (unless (file-ignore-p path)
      (editor::insert-buffer-string point (make-file-line level project path)))))

(defun collect-expanded-truenames (buffer)
  (with-point ((point (buffer-point buffer)))
    (buffer-start point)
    (loop for truename = (get-text-property point :truename)
          when (and truename (entry-expanded-p point))
            collect truename
          do (setq point (valid-entry-offset point 1))
          until (null point))))

(defun restore-expansion-state (buffer truenames)
  (with-point ((point (buffer-point buffer)))
    (buffer-start point)
    (loop
       do (when (equal (get-text-property point :truename) (car truenames))
            (when (not (entry-expanded-p point))
              (expand point (get-text-property point :kind)))
            (pop truenames))
          (setq point (valid-entry-offset point 1))
       until (or (null point) (null truenames)))))

(defun insert-project (point project &optional expand-p)
  (with-point ((point point))
    (editor::insert-buffer-string
     point (make-entry-string (project-name project) 'project-name-face t
                              :kind :project :level 0 :project project :truename (truename (project-path project))))
    (line-offset point -1 0)
    (when expand-p (expand point :project))))

(defun insert-workspace (buffer workspace &optional expand-first)
  (let ((point    (buffer-point buffer))
        (projects (workspace-projects workspace)))
    (if projects
      (loop for i from 0
            for project in projects
            do (insert-project point project (when expand-first (zerop i)))
               (buffer-end point)
               (insert-character point #\Newline))
      (insert-string point (format nil "No project in workspace.~%~%Run \"Side Tree Add Project\"~%to add one.")))))

;; Buffer and window

(defun get-workspace-buffer (workspace)
  (let ((name (string-append "Workspace: " (workspace-name workspace))))
    (or (buffer-from-name name)
        (let ((buffer (make-buffer name :modes '("Side Tree") :flag :side-tree)))
          (setf (buffer-value buffer 'workspace) workspace)
          (with-buffer-writable buffer
            (with-buffer-locked (buffer)
              (insert-workspace buffer workspace t)))
          (buffer-start (buffer-point buffer))
          (setf (buffer-modified buffer) nil)
          buffer))))

(defun refresh-workspace-buffer (workspace)
  (let* ((name   (string-append "Workspace: " (workspace-name workspace)))
         (buffer (buffer-from-name name)))
    (if buffer
      (with-buffer-writable buffer
        (with-buffer-locked (buffer)
          (let ((expanded (collect-expanded-truenames buffer))
                (pos (point-position (buffer-point buffer))))
            (clear-buffer buffer)
            (insert-workspace buffer workspace)
            (restore-expansion-state buffer expanded)
            (setf (point-position (buffer-point buffer)) pos
                  (buffer-modified buffer) nil))))
      (get-workspace-buffer workspace))))

(defun create-side-window ()
  (let* ((window      (current-window))
         (pane        (window-text-pane window))
         (total-width (get-window-width window))
         (ratio       (/ *default-width* total-width)))
    (editor::split-editor-window pane t t)
    (loop do (sleep 0.01)
       until (and (> (length (capi:layout-description
                              (capi:element-parent (capi:element-parent pane))))
                     1)
                  (capi:editor-window
                   (lw-tools::editor-pane-wrapper-pane
                    (car (capi:layout-description
                          (capi:element-parent (capi:element-parent pane))))))))
    (let* ((row-layout  (capi:element-parent (capi:element-parent pane)))
           (side-window (capi:editor-window
                         (lw-tools::editor-pane-wrapper-pane
                          (car (capi:layout-description row-layout))))))
      (capi:apply-in-pane-process
       pane
       (lambda ()
         (setf (capi:layout-ratios row-layout) (list ratio nil (- 1 ratio)))
         (setf (capi:layout-description row-layout) (capi:layout-description row-layout))
         (setf (capi:editor-pane-wrap-style (window-text-pane side-window)) nil)))
      (setf (getf *windows-plist* pane) side-window)
      side-window)))

;; Main commands

(defcommand "Side Tree" (p &optional workspace)
     "Show Side Tree."
     "Show Side Tree. When WORKSPACE is supplied, show this WORKSPACE."
  (declare (ignore p))
  (let* ((pane   (window-text-pane (current-window)))
         (window (or (when (buffer-value (current-buffer) 'workspace)
                       (current-window))
                     (let ((win (getf *windows-plist* pane)))
                       (if (editor::window-alive-p win) win
                         (progn (remf *windows-plist* pane) nil)))
                     (cond ((> (length (capi:layout-description
                                        (capi:element-parent (capi:element-parent pane))))
                               1)
                            (editor-error "Side Tree cannot initialize when there's multiple windows in current editor."))
                           ((not (typep (capi:element-interface pane) 'lw-tools:editor))
                            (editor-error "Side Tree cannot initialize outside of IDE Editor."))
                           (t (create-side-window))))))
    (unless (boundp '*workspaces*)
      (initialize))
    (let ((workspace (or workspace
                         (buffer-value (current-buffer) 'workspace)
                         (car *workspaces*))))
      (unless (equalp (buffer-value (current-buffer) 'workspace) workspace)
        (setf (window-buffer window)
              (get-workspace-buffer workspace))))
    (capi:apply-in-pane-process
     (window-text-pane window)
     (lambda () (capi:set-pane-focus (window-text-pane window))))))

(defcommand "Side Tree Select Window" (p)
     "Switch between the Side Tree and the normal editor window.
If there's no Side Tree in current editor, open a new one."
     "Switch between the Side Tree and the normal editor window.
If there's no Side Tree in current editor, open a new one."
  (declare (ignore p))
  (if (buffer-value (current-buffer) 'workspace)
    (let ((pane (nth (1- (position (current-window) *windows-plist*))
                     *windows-plist*)))
      (capi:apply-in-pane-process pane (lambda () (capi:set-pane-focus pane))))
    (side-tree-command nil)))

;; Simple commands

(defcommand "Side Tree Next Line" (p)
     "Go to next entry in Side Tree. Go back to the top when reached the end of the buffer."
     "Go to next entry in Side Tree. Go back to the top when reached the end of the buffer."
  (let* ((point (current-point))
         (column (editor::set-target-column point)))
    (loop repeat (or p 1)
          do (unless (valid-entry-offset point 1)
               (buffer-start point))
             (editor::move-to-column point column))
    (setf (editor::last-command-type) :line-motion)))

(defcommand "Side Tree Previous Line" (p)
     "Go to previous entry in Side Tree. Go back to the end when reached the top of the buffer."
     "Go to previous entry in Side Tree. Go back to the end when reached the top of the buffer."
  (let* ((point (current-point))
         (column (editor::set-target-column point)))
    (loop repeat (or p 1)
          do (unless (valid-entry-offset point -1)
               (buffer-end point)
               (valid-entry-offset (current-point) -1))
             (editor::move-to-column point column))
    (setf (editor::last-command-type) :line-motion)))

(defcommand "Side Tree Refresh" (p)
     "Redisplay the Side Tree buffer."
     "Redisplay the Side Tree buffer."
  (declare (ignore p))
  (refresh-workspace-buffer
   (or (buffer-value (current-buffer) 'workspace)
       (car *workspaces*)))
  (message "Refresh complete"))

(defcommand "Side Tree Parent Entry" (p)
     "Move current point to the parent entry."
     "Move current point to the parent entry."
  (loop repeat (or p 1)
        do (with-point ((point (current-point)))
             (when-let (level (get-text-property point :level))
               (loop do (setq point (valid-entry-offset point -1))
                        (when (and point (< (get-text-property point :level) level))
                          (move-point (current-point) point)
                          (return)) 
                  until (null point))))))

(defcommand "Side Tree Previous Project" (p)
     "Move current point to the previous project."
     "Move current point to the previous project."
  (loop repeat (if (zerop (get-text-property (current-point) :level))
                 (or p 1)
                 (or (and p (1+ p)) 2))
        do (with-point ((point (current-point)))
             (loop do (setq point (valid-entry-offset point -1))
                      (when (and point (zerop (get-text-property point :level)))
                        (move-point (current-point) point)
                        (return))
                until (null point)))))

(defcommand "Side Tree Next Project" (p)
     "Move current point to the previous project."
     "Move current point to the previous project."
  (loop repeat (or p 1)
        do (with-point ((point (current-point)))
             (loop do (setq point (valid-entry-offset point 1))
                      (when (and point (zerop (get-text-property point :level)))
                        (move-point (current-point) point)
                        (return))
                until (null point)))))

(defcommand "Side Tree Quit" (p)
     "Quit the Side Tree, dismiss the side window."
     "Quit the Side Tree, dismiss the side window."
  (declare (ignore p))
  (editor::delete-window (current-window)))

;; Workspace

(defcommand "Side Tree Add Workspace" (p)
     "Ask for a new workspace to add."
     "Ask for a new workspace to add."
  (declare (ignore p))
  (when-let* ((name (prompt-for-string :prompt "Workspace name: "))
              (workspace (make-workspace :name name)))
    (push-end workspace *workspaces*)
    (save-config)
    (when (buffer-value (current-buffer) 'workspace)
      (setf (window-buffer (current-window)) (get-workspace-buffer workspace)))))

(defcommand "Side Tree Remove Workspace" (p)
     "Ask for a workspace to remove."
     "Ask for a workspace to remove."
  (declare (ignore p))
  (when (= (length *workspaces*) 1)
    (editor-error "Cannot remove the last workspace"))
  (let ((workspace (prompt-for-workspace :prompt "Workspace to remove: ")))
    (setq *workspaces* (delete workspace *workspaces*))
    (save-config)
    (when (buffer-value (current-buffer) 'workspace)
      (setf (window-buffer (current-window))
            (get-workspace-buffer (car *workspaces*))))))

(defcommand "Side Tree Rename Workspace" (p)
     "Ask for a new name for current workspace."
     "Ask for a new name for current workspace."
  (declare (ignore p))
  (let* ((workspace (or (buffer-value (current-buffer) 'workspace)
                        (prompt-for-workspace :prompt "Rename: ")))
         (buffer   (buffer-from-name (string-append "Workspace: " (workspace-name workspace))))
         (new-name (prompt-for-string :prompt "New name: ")))
    (setf (workspace-name workspace) new-name)
    (save-config)
    (when buffer
      (setf (buffer-name buffer) (string-append "Workspace: " new-name)))))

(defcommand "Side Tree Switch Workspace" (p)
     "Switch to another workspace."
     "Switch to another workspace."
  (declare (ignore p))
  (let ((workspace (prompt-for-workspace :prompt "Workspace: ")))
    (side-tree-command nil workspace)))

(defcommand "Side Tree Next Workspace" (p)
     "Switch to next workspace."
     "Switch to next workspace."
  (declare (ignore p))
  (when (= (length *workspaces*) 1)
    (editor-error "There's only one workspace."))
  (when-let (current (buffer-value (current-buffer) 'workspace))
    (let ((pos (position current *workspaces*)))
      (if (= pos (1- (length *workspaces*)))
        (setq pos 0)
        (incf pos))
      (side-tree-command nil (nth pos *workspaces*)))))

;; Project

(defcommand "Side Tree Add Project" (p)
     "Ask for a project to add."
     "Ask for a project to add."
  (declare (ignore p))
  (let* ((workspace (or (buffer-value (current-buffer) 'workspace)
                        (car *workspaces*)))
         (buffer    (buffer-from-name (string-append "Workspace: " (workspace-name workspace))))
         (dir       (prompt-for-file :prompt           "Project root: "
                                     :default          (editor::buffer-default-directory buffer)
                                     :file-directory-p t
                                     :must-exist       t))
         (project   (make-project :name (car (last (pathname-directory dir)))
                                  :path dir)))
    (when (member dir (workspace-projects workspace) :key #'project-path :test #'equal)
      (editor-error "This project has existed in current workspace."))
    (push-end project (workspace-projects workspace))
    (save-config)
    (when buffer
      (with-point ((point (buffer-point buffer)))        
        (with-buffer-writable buffer
          (with-buffer-locked (buffer)
            (buffer-start point)
            (unless (valid-entry-p point)
              (clear-buffer buffer))
            (buffer-end point)
            (insert-project point project)))))))

(defcommand "Side Tree Remove Project" (p)
     "Ask for a project to remove."
     "Ask for a project to remove."
  (declare (ignore p))
  (let* ((workspace (or (buffer-value (current-buffer) 'workspace)
                        (car *workspaces*)))
         (projects  (workspace-projects workspace))
         (project   (prompt-for-project workspace :prompt "Project name: ")))
    (setf (workspace-projects workspace) (delete project projects))
    (save-config)
    (refresh-workspace-buffer workspace)))

(defcommand "Side Tree Rename Project" (p)
     "Ask for a new name for current project."
     "Ask for a new name for current project."
  (declare (ignore p))
  (if-let (project (get-text-property (current-point) :project))
    (let ((new-name (prompt-for-string :prompt "New Name: ")))
      (setf (project-name project) new-name)
      (save-config)
      (refresh-workspace-buffer (buffer-value (current-buffer) 'workspace)))
    (editor-error "There's no project here")))

(defcommand "Side Tree Move Project Up" (p)
     "Move the current project up."
     "Move the current project up."
  (declare (ignore p))
  (let ((point (current-point)))
    (if-let (project (get-text-property point :project))
        (let* ((workspace (buffer-value (current-buffer) 'workspace))
               (pos (position project (workspace-projects workspace))))
          (rotatef (nth pos (workspace-projects workspace))
                   (nth (1- pos) (workspace-projects workspace)))
          (save-config)
          (refresh-workspace-buffer workspace)
          (buffer-start point)
          (loop until (eq (get-text-property point :project) project)
                do (line-offset point 1 0)))
      (editor-error "There's no project here"))))

(defcommand "Side Tree Move Project Down" (p)
     "Move the current project down."
     "Move the current project down."
  (declare (ignore p))
  (let ((point (current-point)))
    (if-let (project (get-text-property point :project))
        (let* ((workspace (buffer-value (current-buffer) 'workspace))
               (pos (position project (workspace-projects workspace))))
          (rotatef (nth pos (workspace-projects workspace))
                   (nth (1+ pos) (workspace-projects workspace)))
          (save-config)
          (refresh-workspace-buffer workspace)
          (buffer-start point)
          (loop until (eq (get-text-property point :project) project)
                do (line-offset point 1 0)))
      (editor-error "There's no project here"))))

;; Actions

(defcommand "Side Tree Toggle" (p)
     "Toggle the expand / collapse of the current entry of the Side Tree"
     "Toggle the expand / collapse of the current entry of the Side Tree"
  (declare (ignore p))
  (let* ((buffer (current-buffer))
         (point  (buffer-point buffer))
         (kind   (get-text-property point :kind)))
    (when kind
      (with-buffer-writable buffer
        (with-buffer-locked (buffer)
          (toggle point kind)))
      (setf (buffer-modified buffer) nil))))

(defcommand "Side Tree Activate" (p)
     "Call ACTIVATE to current entry"
     "Call ACTIVATE to current entry"
  (declare (ignore p))
  (let* ((buffer (current-buffer))
         (point  (buffer-point buffer))
         (kind   (get-text-property point :kind)))
    (when kind
      (with-buffer-writable buffer
        (with-buffer-locked (buffer)
          (activate point kind)))
      (setf (buffer-modified buffer) nil))))

;; File operations

(defcommand "Side Tree Find File" (p)
     "Find file at the location of current Side Tree entry"
     "Find file at the location of current Side Tree entry"
  (declare (ignore p))
  (let* ((point (current-point))
         (pn (prompt-for-file 
              :prompt "Find File: "
              :must-exist nil
              :wildp *find-file-wild-pathname-p*
              :help "Name of file to read into its own buffer."
              :file-directory-p *find-file-file-directory-p*
              :default (if (valid-entry-p point)
                         (pathname-location (get-text-property point :truename))
                         (editor::buffer-default-directory (current-buffer)))))
	 (buffer (if (wild-pathname-p pn)
                   (editor::new-buffer-for-directory pn
                                                     (if (or (pathname-name pn)
                                                             (pathname-type pn))
                                                       nil
                                                       *ignorable-file-suffices*))
                   (editor::find-file-buffer-verbose pn))))
    (when buffer
      (editor::record-active-buffer-pathname buffer :open)
      (let ((editor::*windows-not-to-use* (list (current-window))))
        (editor::goto-buffer buffer nil)))
    buffer))

(defcommand "Side Tree Create Directory" (p)
     "Create a directory at current entry's location."
     "Create a directory at current entry's location."
  (declare (ignore p))
  (let* ((point (current-point))
         (path (prompt-for-file :prompt "Create Directory: "
                                :default (if (valid-entry-p point)
                                           (pathname-location (get-text-property point :truename))
                                           (editor::buffer-default-directory (current-buffer)))
                                :must-exist nil)))
    (if (probe-file path)
      (editor-error (format nil "Cannot create directory ~A: file exists"))
      (progn
        (ensure-directories-exist
         (if (directory-pathname-p path) path
           (merge-pathnames (make-pathname :directory (list :relative (file-namestring path)))
                            (pathname-location path))))
        (when-let (workspace (buffer-value (current-buffer) 'workspace))
          (refresh-workspace-buffer workspace))))))

(defcommand "Side Tree Create File" (p)
     "Create a file at current entry's location."
     "Create a file at current entry's location."
  (declare (ignore p))
  (let* ((point (current-point))
         (path (prompt-for-file :prompt "Create File: "
                                :default (if (valid-entry-p point)
                                           (pathname-location (get-text-property point :truename))
                                           (editor::buffer-default-directory (current-buffer)))
                                :must-exist nil)))
    (if (probe-file path)
      (editor-error (format nil "Cannot create file ~A: file exists"))
      (progn
        (ensure-directories-exist path)
        (unless (directory-pathname-p path)
          (open path :direction :probe :if-does-not-exist :create))
        (when-let (workspace (buffer-value (current-buffer) 'workspace))
          (refresh-workspace-buffer workspace))))))

(defcommand "Side Tree Delete File" (p)
     "Delete the file at current entry"
     "Delete the file at current entry"
  (declare (ignore p))
  (let* ((point (current-point))
         (truename (get-text-property point :truename)))
    (if truename
      (when (and truename (sys::confirm-it (format nil "Delete ~A?" truename)))
        (handler-case
            (if (file-directory-p truename)
              (when (sys::confirm-it (format nil "Recursively delete ~A?" truename))
                (delete-directory-tree truename))
              (delete-file truename t))
          (error (e) (editor-error "Failed to delete ~A: ~A" truename e)))
        (refresh-workspace-buffer (buffer-value (current-buffer) 'workspace))
        (message (format nil "Deleted ~A" truename)))
      (editor-error "Nothing to delete here"))))

(defcommand "Side Tree Rename File" (p)
     "Rename the file at current entry"
     "Rename the file at current entry"
  (declare (ignore p))
  (when-let* ((old (get-text-property (current-point) :truename))
              (new (prompt-for-file :default old
                                    :directory :output
                                    :must-exist nil
                                    :prompt (format nil "Rename ~a to: " (file-namestring old)))))
    (block nil
      (if (directory-pathname-p new)
        (progn
          (if (probe-file new)
            (when (file-directory-p old)
              (if (sys::confirm-it (format nil "Directory ~A exists.  Overwrite it anyway?" new))
                (delete-directory-tree new)
                (return)))
            (unless (file-directory-p old)
              (if (sys::confirm-it (format nil "Directory ~A does not exist.  Create it?" new))
                (ensure-directories-exist new)
                (return))))
          (unless (file-directory-p old)
            (setf new (merge-pathnames (file-namestring old) new))))
        (if (probe-file new)
          (unless (sys::confirm-it (format nil "File ~A exists.  Overwrite it anyway?" new))
            (return))))
      (when (rename-file old new)
        (refresh-workspace-buffer (buffer-value (current-buffer) 'workspace))
        (message "renamed ~a to ~a" (file-namestring old) new)))))

(defcommand "Side Tree Copy File" (p)
     "Copy the file at current entry"
     "Copy the file at current entry"
  (declare (ignore p))
  (when-let* ((old (get-text-property (current-point) :truename))
              (new (prompt-for-file :default old
                                    :directory :output
                                    :must-exist nil
                                    :prompt (format nil "Copy ~a to: " (file-namestring old)))))
    (block nil
      (if (directory-pathname-p new)
        (progn
          (if (probe-file new)
            (when (file-directory-p old)
              (if (sys::confirm-it (format nil "Directory ~A exists.  Overwrite it anyway?" new))
                (delete-directory-tree new)
                (return)))
            (unless (file-directory-p old)
              (if (sys::confirm-it (format nil "Directory ~A does not exist.  Create it?" new))
                (ensure-directories-exist new)
                (return))))
          (unless (file-directory-p old)
            (setf new (merge-pathnames (file-namestring old) new))))
        (if (probe-file new)
          (unless (sys::confirm-it (format nil "File ~A exists.  Overwrite it anyway?" new))
            (return))))
      (when (if (file-directory-p old)
              (copy-directory old new)
              (copy-file old new))
        (refresh-workspace-buffer (buffer-value (current-buffer) 'workspace))
        (message "Copied ~a to ~a" (file-namestring old) new)))))

(defcommand "Side Tree Load File" (p)
     "Try to load the file at current entry. If the file is an ASDF system file, load the systems defined in it."
     "Try to load the file at current entry. If the file is an ASDF system file, load the systems defined in it."
  (declare (ignore p))
  (let* ((truename (get-text-property (current-point) :truename))
         (filename (file-namestring truename)))
    (handler-case
        (if (member (pathname-type truename) '("asd" "asdf") :test #'string-equal)
          (progn
            (message "Loading ASDF system file ~A" filename)
            (load truename :package "ASDF")
            (asdf:map-systems
             (lambda (system)
               (when (equal (asdf:system-source-file system) truename)
                 (asdf:load-system system)))))
          (progn
            (message "Loading ~A" filename)
            (load truename)))
      (error (e) (editor-error "Cannot load ~A: ~A" filename e)))
    (message "~A has been loaded." filename)))

(defcommand "Side Tree Open File Externally" (p)
     "Open the current entry using external program."
     "Open the current entry using external program."
  (declare (ignore p))
  (labels ((find-executable (name)
             (declare (inline find-executable))
             (loop for dir in (split-sequence '(#\:) (environment-variable "PATH"))
                   thereis (car (directory (make-pathname :name name :defaults (truename dir))))))
           (open-file (path)
             (declare (inline open-file))
             (sys:call-system
              (string-append #+mswindows "start"
                             #+darwin "open"
                             #+linux (or (find-executable "xdg-open") "open")
                             " " (namestring (truename path))))))
    (when-let (truename (get-text-property (current-point) :truename))
      (open-file truename))))

;; Copy pathnames

(defcommand "Side Tree Copy Project Path" (p)
     "Copy current project's namestring"
     "Copy current project's namestring"
  (when-let (project (get-text-property (current-point) :project))
    (let ((str (namestring (truename (project-path project)))))
      (editor::save-kill-text p str)
      (message "Copied project path: ~A" str))))

(defcommand "Side Tree Copy Absolute Path" (p)
     "Copy current entry's namestring"
     "Copy current entry's namestring"
  (when-let (truename (get-text-property (current-point) :truename))
    (let ((str (namestring truename)))
      (editor::save-kill-text p str)
      (message "Copied absolute path: ~A" str))))

(defcommand "Side Tree Copy Relative Path" (p)
     "Copy current entry's enough namestring relative to current project."
     "Copy current entry's enough namestring relative to current project."
  (when-let (truename (get-text-property (current-point) :truename))
    (let ((str (enough-namestring
                truename
                (project-path (get-text-property (current-point) :project)))))
      (editor::save-kill-text p str)
      (message "Copied relative path: ~A" str))))

(defmode "Side Tree"
  :major-p t
  :setup-function (lambda (buffer) (setf (buffer-writable buffer) nil))
  :key-bindings '(("Side Tree Previous Line"        "p")
                  ("Side Tree Next Line"            "n")
                  ("Side Tree Previous Line"        "C-p")
                  ("Side Tree Next Line"            "C-n")
                  ("Side Tree Parent Entry"         "u")
                  ("Side Tree Refresh"              "g")
                  ("Side Tree Quit"                 "q")
                  ("Side Tree Toggle"               "Tab")
                  ("Side Tree Activate"             "Return")
                  ("Side Tree Find File"            #("C-x" "C-f"))
                  ("Side Tree Create Directory"     "+")
                  ("Side Tree Create Directory"     #("c" "d"))
                  ("Side Tree Create File"          #("c" "f"))
                  ("Side Tree Copy File"            "C")
                  ("Side Tree Copy File"            #("y" "f"))
                  ("Side Tree Copy Project Path"    #("y" "p"))
                  ("Side Tree Copy Absolute Path"   #("y" "a"))
                  ("Side Tree Copy Relative Path"   #("y" "r"))
                  ("Side Tree Rename File"          "R")
                  ("Side Tree Delete File"          "d")
                  ("Side Tree Load File"            "L")
                  ("Side Tree Open File Externally" "E")
                  ("Side Tree Add Project"          #("C-c" "C-p" "a"))
                  ("Side Tree Remove Project"       #("C-c" "C-p" "d"))
                  ("Side Tree Rename Project"       #("C-c" "C-p" "r"))
                  ("Side Tree Previous Project"     "C-k")
                  ("Side Tree Next Project"         "C-j")
                  ("Side Tree Move Project Up"      "M-Up")
                  ("Side Tree Move Project Down"    "M-Down")
                  ("Side Tree Add Workspace"        #("C-c" "C-w" "a"))
                  ("Side Tree Remove Workspace"     #("C-c" "C-w" "d"))
                  ("Side Tree Rename Workspace"     #("C-c" "C-w" "r"))
                  ("Side Tree Switch Workspace"     #("C-c" "C-w" "s"))
                  ("Side Tree Next Workspace"       #("C-c" "C-w" "n"))))

;; Recommend binding
(bind-key "Side Tree Select Window" "M-0")
