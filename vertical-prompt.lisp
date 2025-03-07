;; Copyright (c) 2025, April & May
;; SPDX-License-Identifier: 0BSD

;; Vertical prompting completion for LW Editor,
;; with fuzzy-matching and marginalia.

;; Like Helm / Ivy / Vertico in Emacs

;; Nerd-icon support included: Load the lw-plugins/nerd-icons-vertical-prompt system

;; TODO:
;; - Support isearch
;; - Support goto line and definition

(defpackage vprompt
  (:use :editor)
  (:export
   *vertical-prompt-mode*
   *vertical-prompt-default-display-count*
   *vertical-prompt-default-marginalia-gap*

   selected-face
   command-binding-face
   marginalia-face
   prompt-face

   make-candidate candidate-string candidate-display-string candidate-marginalia
   make-vertical-parse-inf
   candidate-func-from-candidates
   
   vertical-parse
   vertical-prompt-for-command
   vertical-prompt-for-keyword
   vertical-prompt-for-variable
   vertical-prompt-for-package
   vertical-prompt-for-buffer
   vertical-prompt-for-file

   echo-area-beginning-command
   echo-area-end-command
   echo-area-previous-command
   echo-area-next-command
   echo-area-scroll-up-command
   echo-area-scroll-down-command
   goto-definition-command)
  (:add-use-defaults))

(in-package vprompt)

(defvar *vertical-prompt-mode* t
  "Replace PROMPT-FOR-* functions to vertical prompting versions.")

(defvar *vertical-prompt-default-display-count* 15
  "Number of candidates shown in the echo-area.

Should not be more than 29, or some display problem may happen.")

(defvar *vertical-prompt-default-marginalia-gap* 2
  "Minimal gap between display-string and marginalia.")

(make-face 'selected-face
           :background (editor::create-dark-background-switchable-color :gray80 :gray20)
           :bold-p t
           :if-exists :overwrite)

(make-face 'command-binding-face
           :italic-p t
           :foreground (editor::create-dark-background-switchable-color :orchid :pink)
           :if-exists :overwrite)

(make-face 'marginalia-face
           :foreground (editor::create-dark-background-switchable-color :dark-blue :lightblue)
           :if-exists :overwrite)

(make-face 'prompt-face
           :foreground (editor::create-dark-background-switchable-color :slateblue3 :darkolivegreen2)
           :if-exists :overwrite)


(defvar *lisp-definition-symbols*
  '(defun defadvice defgeneric defmethod
     defvar defparameter defconstant define-editor-variable fli:define-foreign-variable
     deftype defstruct defclass defpackage defmode define-condition
     defmacro define-symbol-macro define-compiler-macro define-modify-macro defsetf define-setf-expander
     capi:define-interface capi:define-layout capi:define-menu capi:define-command
     defcommand define-editor-variable define-editor-mode-variable
     define-action
     color:define-color-alias make-face
     dspec:def))

(defstruct candidate
  string         ; A CL:STRING, will be used to replace user's existing input when pressed #\Tab
  display-string ; string or buffer-string being displayed
  marginalia)    ; CL:STRING for the marginalia

;; Since the editor::parse-inf has been shaked so we cannot include it...
(defstruct vertical-parse-inf
  ;; Must be provided
  candidate-func      ; function being called with echo-area's input, yield a sequence of candidate
  (display-count 10)
  (marginalia-gap 2)
  ;; Computed
  selected            ; Candidate being selected
  matched-candidates) ; Return value of candidate-func

;; Buffer local variables used in the echo-area:
;; - vertical-parse-inf
;; - select-input-overlay

;; Candidate functions

(defun candidate-func-from-candidates (candidates)
  "Pack a list of candidates to a closure for :CANDIDATE-FUNC, use fuzzy matching."
  (lambda (input)
    (if (plusp (length input))
      (let (result)
        (dotimes (i (length candidates))
          (let* ((cand (elt candidates i))
                 (string (editor::buffer-string-string (candidate-string cand))))
            (when-let (starts (fuzzy-search input string))
              (push (list cand starts string) result))))
        (stable-sort result
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
        (mapcar #'first result))
      candidates)))

(defun file-candidate-function (&optional allow-directory only-directory)
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
      (mapcar (lambda (path)
                (setq path (truename path))
                (make-candidate
                 :string (namestring path)
                 :display-string (or (file-namestring path)
                                     (string-append (car (last (pathname-directory path))) "/"))))
              result))))

(defun buffer-candidate-function (buffers)
  (candidate-func-from-candidates
   (mapcar (lambda (buf)
             (make-candidate
              :string (buffer-name buf)
              :display-string (buffer-name buf)
              :marginalia (if (buffer-pathname buf)
                            (namestring (buffer-pathname buf))
                            "")))
           buffers)))

(defun command-candidate-function ()
  (let ((table (slot-value editor::*command-names* 'editor::table))
        result)
    (dotimes (i (slot-value editor::*command-names* 'editor::num-entries))
      (let* ((entry (aref table i))
             (name (slot-value entry 'editor::proper))
             (obj (slot-value entry 'editor::value))
             (buffer-string (editor::make-buffer-string
                             :%string name
                             :properties `((0 ,(length name) (editor:face editor::default))))))
        (when-let (bindings (editor::filter-hidden-command-bindings
                             obj
                             (editor::command-bindings obj)
                             (current-buffer)))
          (let ((str (with-output-to-string (out)
                       (editor::print-some-keys (list (caar bindings)) out))))
            (setq buffer-string
                  (editor::concatenate-buffer-strings
                   buffer-string
                   (editor::make-buffer-string
                    :%string (string-append "  (" str ")")
                    :properties `((0 ,(+ (length str) 4) (editor:face command-binding-face))))))))
        (push (make-candidate
               :string name
               :display-string buffer-string
               ;; For some commands there may be: "Warning: Unspecialized DOCUMENTATION method called with..."
               ;; Muffle them.
               :marginalia (handler-case
                               (editor::command-documentation (slot-value entry 'editor::value))
                             (warning (w) nil)))
              result)))
    (candidate-func-from-candidates
     (sort result
           (lambda (x1 x2)
             (string< (candidate-string x1)
                      (candidate-string x2)))))))


;; Displaying functions

;; Same with flex-complete.lisp
(defun fuzzy-search (target source)
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

(defun buffer-string-subseq (buffer-string start &optional end)
  (let* ((str (editor::buffer-string-string buffer-string))
         (props (editor::buffer-string-properties buffer-string))
         (cut-end-p (and end (/= end (length str)))))
    (unless (zerop start)
      (setq str (subseq str start)
            props
            (loop for (ps pe prop) in props
                  for new-start = (- ps start)
                  for new-end = (- pe start)
                  when (plusp new-end)
                    collect (list (min 0 new-start) new-end prop))))
    (when cut-end-p
      (setq str (subseq str 0 end))
      (loop for i from 0
            for prop in props
            when (> (second prop) end)
              if (> (first prop) end)
                do (setq props (when (plusp i)
                                 (subseq props 0 (1- i))))
                   (return)
              else do (setf (second (nth i props)) end)
                      (setq props (subseq props 0 i))))
    (editor::make-buffer-string :%string str :properties props)))

(defun vertical-parse-update (buffer)
  "(Re)Display updated echo area content when change have made."
  (let ((inf (buffer-value buffer 'vertical-parse-inf)))
    (with-slots (selected matched-candidates display-count marginalia-gap) inf
      (with-buffer-locked (buffer)
        (set-buffer-after-change-hook buffer 'vertical-parse-after-change nil)
        (let* ((index (position selected matched-candidates))
               (min-index (max 0 (min (- (or index 0) (- display-count (1+ (floor display-count 2))))
                                      (- (length matched-candidates) display-count))))
               (max-index (min (length matched-candidates) (+ min-index display-count)))
               (window-width (get-window-width (editor::current-echo-area-window)))
               (marginalia-start (+ (loop for i from (max 0 (- min-index 50))
                                            below (min (+ max-index 50) (length matched-candidates))
                                          maximize (length (editor::buffer-string-string
                                                            (candidate-display-string
                                                             (elt matched-candidates i)))))
                                    3
                                    marginalia-gap)))
          ;; Delete old content
          (with-point ((point (buffers-start buffer)))
            (when (next-single-property-change point 'vertical-displayer)
              (delete-between-points (buffers-start buffer) point)))
          (with-point ((point (buffers-start buffer) :after-insert))
            (do ((i min-index (1+ i)))
                ((= i max-index))
              (let ((cand (elt matched-candidates i)))
                (with-point ((start point :before-insert))
                  ;; Indicator
                  (insert-things point
                                 #\Space (if (eql i index) (code-char 9654) #\Space) #\Space)
                  ;; Display string
                  (let ((display-str (candidate-display-string cand)))
                    (editor::insert-buffer-string
                     point
                     (buffer-string-subseq display-str 0 (min (length (editor::buffer-string-string display-str))
                                                              (- window-width 3)))))
                  ;; Marginalia
                  (when-let (ma (candidate-marginalia cand))
                    (setq ma (first (split-sequence
                                     '(#\Newline)
                                     (subseq ma 0 (min (length ma)
                                                       (- window-width marginalia-start 1))))))
                    (editor::insert-spaces point (- marginalia-start (point-column point)))
                    (editor::insert-buffer-string
                     point
                     (editor::make-buffer-string :%string ma
                                                 :properties `((0 ,(length ma) (editor:face marginalia-face))))))
                  (insert-character point #\Newline)
                  (when (eql i index)
                    (editor::push-region-face 'selected-face start point nil)))))
            ;; Top margin
            (when (< (- max-index min-index) display-count)
              (let ((end (copy-point (buffers-start buffer)))
                    (count (- display-count (- max-index min-index))))
                (editor::insert-buffer-string
                 end
                 (editor::make-buffer-string :%string (make-string count :initial-element #\Newline)
                                             :properties `((0 ,count (editor:face editor::default)))))))
            ;; Candidate count
            (let ((count-str (format nil "(~A/~A) "
                                     (if index (1+ index) "*")
                                     (length matched-candidates))))
              (editor::insert-buffer-string
               point
               (editor::make-buffer-string :%string count-str
                                           :properties `((0 ,(length count-str) (editor:face prompt-face))))))
            ;; Selection overlay
            (if index
              (when-let (ov (buffer-value buffer 'select-input-overlay))
                (delete-overlay ov)
                (setf (buffer-value buffer 'select-input-overlay) nil))
              (unless (buffer-value buffer 'select-input-overlay)
                (let ((ov (make-overlay (editor::parse-starting-point buffer) (buffers-end buffer)
                                        :end-kind :after-insert)))
                  (overlay-put ov 'editor:face 'selected-face)
                  (setf (buffer-value buffer 'select-input-overlay) ov))))
            (put-text-property (buffers-start buffer) point 'vertical-displayer t)))
        (set-buffer-after-change-hook buffer 'vertical-parse-after-change t)))))

(defun vertical-parse-after-change (buffer &rest ignore)
  "Update the echo area after any user input."
  (declare (ignore ignore))
  (let ((inf (buffer-value buffer 'vertical-parse-inf))
        (parse-inf (editor::current-parsing-information buffer)))
    (when (and inf parse-inf
               (with-point ((pt (buffers-start buffer)))
                 (next-single-property-change pt 'vertical-displayer)))
      (with-slots (matched-candidates selected) inf
        (let ((input (editor::echo-area-input :buffer buffer)))
          (setf matched-candidates
                (funcall (vertical-parse-inf-candidate-func inf) input))
          (let ((default (slot-value parse-inf 'editor::default-string)))
            (if matched-candidates
              (if (or (and default
                           (zerop (length input))
                           (plusp (length default)))
                      (equalp input default))
                (setf selected (find default matched-candidates :key #'candidate-string))
                (when (or (zerop (length input))
                          (and matched-candidates
                               (not (member selected matched-candidates))))
                  (setf selected (elt matched-candidates 0))))
              (setf selected nil))))
        (vertical-parse-update buffer)))))


;; Main parse function
;; Modified from PARSE-FOR-SOMETHING

(defun vertical-parse (&rest options
                             &key candidates candidate-func
                             (display-count *vertical-prompt-default-display-count*)
                             (marginalia-gap *vertical-prompt-default-marginalia-gap*)
                             not-editor
                             &allow-other-keys)
  "Parse for something with vertical candidates. Capable with editor::parse-for-something.

If candidate-func provided, it should be a function that takes user's
input string and return a list of matched candidates;

If candidates or string-table provided, generate candidate-func from them."
  (let* ((cwindow (current-window))
         (cbuffer (when cwindow (window-buffer cwindow))))
    (when (and cbuffer
               (mp:lock-owned-by-current-process-p (editor::buffer-lock cbuffer)))
      (error "Parse-for-something is called with the current buffer ~s locked" cbuffer))
    (let* ((echo-area-window (editor::check-echo-area cwindow))
           (echo-area-buffer (and echo-area-window (window-buffer echo-area-window)))
           (echo-area-stream (and echo-area-buffer (editor::buffer-stream echo-area-buffer))))
      (when (editor::current-parsing-information echo-area-buffer)
        (editor::already-parsing-error))
      (unless (eq cbuffer echo-area-buffer)
        (editor::set-buffer-parent-buffer echo-area-buffer cbuffer))
      (editor::resize-echo-area echo-area-window)
      (let* ((editor::*editor-state* (editor::make-editor-state))
             (editor::*can-use-echo-area* t)
             (func (cond (candidate-func candidate-func)
                         (candidates (candidate-func-from-candidates candidates))
                         ((getf options :string-tables)
                          (candidate-func-from-candidates
                           (mapcan (lambda (table)
                                     (if (hash-table-p table)
                                       (loop for key being each hash-key of table
                                             collect (make-candidate
                                                      :string (princ-to-string key)
                                                      :display-string (if (symbolp key)
                                                                        (string-downcase key)
                                                                        (princ-to-string key))))
                                       (loop for entry across table
                                             collect (make-candidate
                                                      :string (slot-value entry 'editor::proper)
                                                      :display-string (slot-value entry 'editor::proper)))))
                                   (getf options :string-tables))))
                         (t #'false)))
             (vparse-inf (make-vertical-parse-inf
                          :candidate-func func
                          :display-count display-count
                          :marginalia-gap marginalia-gap)))
        (declare (dynamic-extent editor::*editor-state*))
        (setf (buffer-value echo-area-buffer 'vertical-parse-inf) vparse-inf)
        (editor::set-current-parsing-information
         (apply 'editor::make-parse-inf :current-window cwindow options)
         echo-area-buffer)
        (let ((editor::*current-parse-inf* (editor::current-parsing-information echo-area-buffer))
              (parse-starting-point (editor::parse-starting-point echo-area-buffer))
              old-parse-starting-point)
          (unwind-protect
              (progn
                (editor::push-echo-process :echo-area echo-area-window)
                (editor::clear-echo-area-buffer echo-area-buffer)
                (let ((editor::*echo-area-buffer* echo-area-buffer)
                      (editor::*echo-area-window* echo-area-window))
                  (setq old-parse-starting-point (copy-i-point parse-starting-point))
                  (buffer-start parse-starting-point)
                  (editor::display-echo-area-prompt echo-area-stream echo-area-window nil nil)

                  ;; For some unknown reason, we must claim the display area we need as soon as possible
                  ;; after PUSH-ECHO-PROCESS being called.
                  (with-point ((point (buffers-start echo-area-buffer) :after-insert))
                    (insert-string point (make-string display-count :initial-element #\Newline))
                    (editor::push-region-face 'prompt-face point parse-starting-point nil)
                    (put-text-property (buffers-start echo-area-buffer) point 'vertical-displayer t))
                  ;; Wait for PUSH-ECHO-PROCESS..
                  (sleep 0.01)
                  (vertical-parse-after-change echo-area-buffer)
                  
                  (when *pop-echo-area*
                    (editor::give-echo-area-focus editor::*echo-area-window*))
                  (editor::use-buffer-and-window echo-area-buffer echo-area-window
                    (editor::recursive-edit nil))))
            (progn
              (setf (buffer-value echo-area-buffer 'vertical-parse-inf) nil)
              (when-let (ov (buffer-value echo-area-buffer 'select-input-overlay))
                (delete-overlay ov)
                (setf (buffer-value echo-area-buffer 'select-input-overlay) nil))
              (set-buffer-after-change-hook echo-area-buffer 'vertical-parse-after-change nil)
              (when (editor::good-point-p old-parse-starting-point)
                (move-point parse-starting-point old-parse-starting-point)
                (delete-point old-parse-starting-point))
              (editor::exiting-echo-cleanup echo-area-window not-editor))))))))


;; Extra commands & advices for vertical-prompting echo area

(defadvice (complete-input-command lw-plugins :around) (p)
  (let ((buffer (editor::current-echo-area-buffer)))
    (if-let (inf (buffer-value buffer 'vertical-parse-inf))
        (with-slots (selected) inf
          (when selected
            (editor::replace-echo-region (candidate-string selected))))
      (call-next-advice p))))

(defadvice (confirm-parse-command lw-plugin :around) (p)
  (let* ((buffer (editor::current-echo-area-buffer))
         (string (editor::echo-area-input :buffer buffer))
         (parse-inf (editor::current-parsing-information))
         (res nil))
    (if-let (inf (buffer-value buffer 'vertical-parse-inf))
        (with-slots (selected) inf
          (unless parse-inf 
            (editor::reset-display-mode)
            (editor-error "Return typed when nothing was being parsed"))
          (editor::do-buffer-before-command-hooks buffer nil nil)
          
          (when selected
            (setq string (candidate-string selected)))
          
          (when (or (zerop (ring-length editor::*echo-area-history*))
                    (string/= string (ring-ref editor::*echo-area-history* 0)))
            (ring-push string editor::*echo-area-history*))
            
          (multiple-value-bind (e-res flag)
              (ignore-errors
                (funcall (slot-value parse-inf 'editor::verify-func)
                         string parse-inf))
            (unless (or e-res flag) 
              (editor-error))
            (when (eq flag :no-value)
              (editor::return-to-input))
            (if (stringp flag)
              (editor-error flag string)
              (when (and (not res) (typep flag 'condition))
                (editor-error flag)))
            (setq res e-res))
          (editor::set-current-parsing-information nil)
          (if (editor::in-recursive-edit)
            (editor::exit-recursive-edit res)))
      (call-next-advice p))))

(defcommand "Echo Area Previous" (p)
     "Select previous candidate if vertical parsing enabled."
     "Select previous candidate if vertical parsing enabled."
  (let* ((buffer (editor::current-echo-area-buffer))
         (inf (buffer-value buffer 'vertical-parse-inf)))
    (if (and inf (editor::current-parsing-information buffer))
      (with-slots (selected matched-candidates) inf
        (loop repeat (or p 1)
              do (if selected
                   (let ((index (position selected matched-candidates)))
                     (setf selected (if (zerop index) nil
                                      (elt matched-candidates (1- index)))))
                   (when matched-candidates
                     (setf selected (elt matched-candidates (1- (length matched-candidates)))))))
        (vertical-parse-update buffer))
      (previous-line-command p))))

(defcommand "Echo Area Next" (p)
     "Select next candidate if vertical parsing enabled."
     "Select next candidate if vertical parsing enabled."
  (let* ((buffer (editor::current-echo-area-buffer))
         (inf (buffer-value buffer 'vertical-parse-inf)))
    (if (and inf (editor::current-parsing-information buffer))
      (with-slots (selected matched-candidates) inf
        (loop repeat (or p 1)
              do (if selected
                   (let ((index (position selected matched-candidates)))
                     (setf selected (if (= index (1- (length matched-candidates))) nil
                                      (elt matched-candidates (1+ index)))))
                   (when matched-candidates
                     (setf selected (elt matched-candidates 0)))))
        (vertical-parse-update buffer))
      (next-line-command p))))

(defcommand "Echo Area Scroll Up" (p)
     "Scroll up a page of candidate if vertical parsing enabled."
     "Scroll up a page of candidate if vertical parsing enabled."
  (let* ((buffer (editor::current-echo-area-buffer))
         (inf (buffer-value buffer 'vertical-parse-inf)))
    (if (and inf (editor::current-parsing-information buffer))
      (with-slots (display-count) inf
        (echo-area-previous-command (* display-count (or p 1))))
      (previous-line-command p))))

(defcommand "Echo Area Scroll Down" (p)
     "Scroll down a page of candidate if vertical parsing enabled."
     "Scroll down a page of candidate if vertical parsing enabled."
  (let* ((buffer (editor::current-echo-area-buffer))
         (inf (buffer-value buffer 'vertical-parse-inf)))
    (if (and inf (editor::current-parsing-information buffer))
      (with-slots (display-count) inf
        (echo-area-next-command (* display-count (or p 1))))
      (previous-line-command p))))

(defcommand "Echo Area Beginning" (p)
     "Select the first candidate if vertical parsing enabled."
     "Select the first candidate if vertical parsing enabled."
  (let* ((buffer (editor::current-echo-area-buffer))
         (inf (buffer-value buffer 'vertical-parse-inf)))
    (if (and inf (editor::current-parsing-information buffer))
      (with-slots (selected matched-candidates) inf
        (when matched-candidates
          (setf selected (elt matched-candidates 0)))
        (vertical-parse-update buffer))
      (beginning-of-buffer-command p))))

(defcommand "Echo Area End" (p)
     "Select the last candidate if vertical parsing enabled."
     "Select the last candidate if vertical parsing enabled."
  (let* ((buffer (editor::current-echo-area-buffer))
         (inf (buffer-value buffer 'vertical-parse-inf)))
    (if (and inf (editor::current-parsing-information buffer))
      (with-slots (selected matched-candidates) inf
        (when matched-candidates
          (setf selected (elt matched-candidates (1- (length matched-candidates)))))
        (vertical-parse-update buffer))
      (end-of-buffer-command p))))

(bind-key "Echo Area Previous" "C-p" :mode "Echo Area")
(bind-key "Echo Area Next" "C-n" :mode "Echo Area")
(bind-key "Echo Area Scroll Up" "M-v" :mode "Echo Area")
(bind-key "Echo Area Scroll Down" "C-v" :mode "Echo Area")
(bind-key "Echo Area Beginning" "M-<" :mode "Echo Area")
(bind-key "Echo Area End" "M->" :mode "Echo Area")


;; Vertical version commands corresponding to PROMPT-FOR-*

(defun vertical-prompt-for-buffer (&key (must-exist t)
                                        default
                                        (ignore-flagged t)
                                        default-string
                                        (prompt "Buffer: ")
                                        (help "Type a buffer name.")
                                        &allow-other-keys)
  "Prompts for a buffer name and return the corresponding buffer.  If
  :must-exist is Nil, then we return the input string."
  (vertical-parse
   :prompt prompt
   :must-exist must-exist
   :help help
   :default default
   :default-string (or default-string (if default (buffer-name default)))
   :complete-field t
   :verify-func
   #'(lambda (string parse-inf)
       (declare (type editor::parse-inf parse-inf))
       (if (and (zerop (length string))
                (or (parse-inf-default parse-inf)
                    (not (parse-inf-must-exist parse-inf))))
         (values (parse-inf-default parse-inf) t)
 
         (if (parse-inf-must-exist parse-inf)
           (multiple-value-bind (ignore buffer won)
               (editor::editor-complete-string string
                                               (list *buffer-names*))
             (declare (ignore ignore won))
             buffer)
           (get-buffer string))))
   :type :keyword
   :flag (if ignore-flagged 
           (editor::selectable-buffers)
           *buffer-list*)
   :default-in-prompt (if default t :none)
   :tag 'buffer
   :complete-func 'editor::complete-buffer-name

   :candidate-func (buffer-candidate-function
                    (if ignore-flagged
                      (editor::selectable-buffers)
                      *buffer-list*))))

(defun vertical-prompt-for-file (&key (direction :input)
                                      (must-exist (not (eq direction :output)))
                                      (create-directories (eq direction :output))
                                      default
                                      default-string
                                      verify-func
                                      ((:ignorable-suffices
                                        *ignorable-file-suffices*)
                                       *ignorable-file-suffices*)
                                      (prompt  "File name: ")
                                      file-directory-p
                                      (wildp nil)
                                      (help  "Type a file name.")
                                      accept-empty-p
                                      
                                      candidate-func
                                      &allow-other-keys)
  "Prompts for a file name and return it."
  (vertical-parse
   :prompt prompt
   :must-exist (if must-exist t
                 (if accept-empty-p nil
                   :no-nil))
   :help help
   :complete-field t
   :default-string (or default-string
                       (if (pathnamep default) 
                         (namestring (pathname-location default))
                         default))
   :verify-func (or verify-func
                    #'(lambda (string parse-inf)
                        (editor::file-prompt-verify
                         string parse-inf
                         :filep t :wildp wildp
                         :file-directory-p file-directory-p
                         :create-directories create-directories)))
   :type :file
   :complete-func 'echo-complete-file
   :default-in-prompt (if default nil :none)
   :default default
   :nf-complete-func 'echo-non-focus-complete-file
   
   :candidate-func (or candidate-func (file-candidate-function file-directory-p))))

(defun vertical-prompt-for-directory (&rest args)
  (apply 'vertical-prompt-for-file
         :prompt (or (getf args :prompt) "Directory: ")
         :default (editor::buffer-default-directory (current-buffer))
         :file-directory-p t
         
         :candidate-func (file-candidate-function t t)
         args))

(defun vertical-prompt-for-variable (&key (must-exist t)
                                          default 
                                          default-string
                                          (prompt  "Variable: ")
                                          (help "Type the name of a variable.")
                                          &allow-other-keys)
  "Prompts for a variable defined in the current scheme of things."
  (let ((tables
         (list (editor::buffer-variables editor::*current-buffer*)
               editor::*global-variable-names*)))
    (vertical-parse
     :verify-func 'editor::variable-verification-function
     :complete-func 'editor::variable-complete-function
     :type :keyword
     :tag 'variable
     :help help
     :must-exist must-exist
     :string-tables tables
     :default default
     :default-string default-string
     :prompt prompt

     :candidate-func (candidate-func-from-candidates
                      (mapcan (lambda (table)
                                (loop for key being each hash-key of table using (hash-value obj)
                                      collect (make-candidate
                                               :string (string-downcase key)
                                               :display-string (string-downcase key)
                                               :marginalia (handler-case
                                                               (editor::variable-object-documentation obj)
                                                             (warning (w) nil)))))
                              tables)))))

(defun vertical-prompt-for-package (&key (must-exist  t)
				(default *package*)
				(prompt  "package: ")
				(help  "Type a package name.")
                                &allow-other-keys)
  "Prompts for a buffer name and return the corresponding buffer.  If
  :must-exist is Nil, then we return the input string."
  (vertical-parse
   :prompt prompt
   :must-exist must-exist
   :help help
   :default default
   :verify-func  'editor::verify-package-func
   :type :keyword
   :complete-field t
   :complete-func 'editor::complete-package-name
   :tag 'package
   :default default

   :candidates (sort (mapcar (lambda (x)
                               (make-candidate :string (string-downcase (package-name x))
                                               :display-string (string-downcase (package-name x))))
                             (list-all-packages))
                     (lambda (x1 x2)
                       (string< (candidate-string x1)
                                (candidate-string x2))))))

(defun vertical-prompt-for-keyword (string-tables
                           &key
                           (must-exist t)
                           default
                           default-string 
                           (prompt  "Keyword: ")
                           (help  "Type a keyword.")
                           tag
                           &allow-other-keys)
  "Prompts for a keyword using the String Tables."
  
  (vertical-parse
   :type :keyword
   :verify-func 'editor::keyword-verification-function
   :must-exist must-exist
   :default default
   :prompt prompt
   :help help
   :tag tag
   :string-tables string-tables
   :default-string default-string))

(defun vertical-prompt-for-command (&rest args
                                          &key prompt
                                          help
                                          &allow-other-keys)
  (declare (ignorable args))
  (vertical-parse
   :type :keyword
   :verify-func 'editor::keyword-verification-function
   :must-exist t
   :prompt (or prompt "Enter Command: ")
   :help (or help "Enter a command")
   :tag 'command
   :string-tables (list editor::*command-names*)
   
   :candidate-func (command-candidate-function)))


;; Advice for overriding original LW PROMPT-FOR-* functions

(defadvice (prompt-for-file lw-plugins :around) (&rest args)
  (if *vertical-prompt-mode*
    (apply 'vertical-prompt-for-file args)
    (apply #'call-next-advice args)))

(defadvice (prompt-for-directory lw-plugins :around) (&rest args)
  (if *vertical-prompt-mode*
    (apply 'vertical-prompt-for-directory args)
    (apply #'call-next-advice args)))

(defadvice (prompt-for-buffer lw-plugins :around) (&rest args)
  (if *vertical-prompt-mode*
    (apply 'vertical-prompt-for-buffer args)
    (apply #'call-next-advice args)))

(defadvice (prompt-for-package lw-plugins :around) (&rest args)
  (if *vertical-prompt-mode*
    (apply 'vertical-prompt-for-package args)
    (apply #'call-next-advice args)))

(defadvice (prompt-for-variable lw-plugins :around) (&rest args)
  (if *vertical-prompt-mode*
    (apply 'vertical-prompt-for-variable args)
    (apply #'call-next-advice args)))

(defadvice (prompt-for-keyword lw-plugins :around) (&rest args)
  (if *vertical-prompt-mode*
    (apply 'vertical-prompt-for-keyword args)
    (apply #'call-next-advice args)))

(defadvice (prompt-for-command lw-plugins :around) (&rest args)
  (if *vertical-prompt-mode*
    (apply 'vertical-prompt-for-command args)
    (apply #'call-next-advice args)))


;; Extra

(defun string-prefix-p (prefix str)
  (when (> (length str) (length prefix))
    (string-equal prefix str :end2 (length prefix))))

;; Personally I don't want to see a bunch of files in my candidates
;; when I asking for a directory, although Emacs is also like this.
(defadvice (editor::list-directory-command lw-plugins :around) (p &optional directory)
     "List a directory in Directory mode."
     "List a directory in Directory mode."
  (if *vertical-prompt-mode*
    (let* ((dir (or directory
                    (vertical-prompt-for-directory
                     :prompt "List Directory: "
                     :must-exist nil
                     :wildp t
                     :help "Name of directory or wildcard file pattern to read into its own buffer."
                     :file-directory-p t
                     :default (editor::buffer-default-directory (current-buffer)))))
           (buffer-dir (if (wild-pathname-p dir)
                         dir
                         (or (probe-file dir)
                             dir)))
           (buffer (or (editor::find-directory-mode-buffer buffer-dir nil)
                       (editor::new-buffer-for-directory buffer-dir nil))))
      (editor::goto-buffer-if-unflagged-current t buffer t))
    (call-next-advice p directory)))

(defcommand "Goto Definition" (p)
     "Find and goto Lisp definition form in current buffer"
     "Find and goto Lisp definition form in current buffer"
  (declare (ignore p))
  (let* ((buffer (current-buffer))
         (*package* (editor::buffer-package-to-use (current-point)))
         (candidates (make-array 10 :fill-pointer 0 :adjustable t))
         (digits (1+ (floor (log (max 1 (editor::buffer-newlines-count buffer)) 10)))))
    (with-buffer-locked (buffer :for-modification nil)
      (with-point ((point (buffers-start buffer))
                   (end (buffers-start buffer)))
        (loop for r = t then (line-offset point 1 0)
              while r
              do (skip-whitespace point)
                 (move-point end point)
                 (line-end end)
                 (let ((str (points-to-string point end)))
                   (when (and (> (length str) 2)
                              (eql (char str 0) #\())
                     (let ((sub (subseq str 1)))
                       (when (some (lambda (sym)
                                     (string-prefix-p (prin1-to-string sym) sub))
                                   *lisp-definition-symbols*)
                         (vector-push-extend
                          (let ((num-str (princ-to-string (1+ (count-lines (buffers-start buffer) point)))))
                            (setq num-str (string-append num-str (make-string (1+ (- digits (length num-str)))
                                                                              :initial-element #\Space)))
                            (make-candidate
                             :string (string-append num-str " " str)
                             :display-string
                             (editor::concatenate-buffer-strings
                              (editor::make-buffer-string
                               :%string num-str
                               :properties `((0 ,(length num-str) (editor:face editor::default))))
                              (editor::make-buffer-string
                               :%string str
                               :properties (editor::bounded-text-properties-in point end :modification nil)))))
                          candidates))))))))
    (let ((selected (vertical-parse :prompt "Goto Definition: "
                                    :candidates candidates
                                    :verify-func (lambda (str inf) (declare (ignore inf)) str))))
      (goto-line buffer (parse-integer selected :junk-allowed t))
      (skip-whitespace (buffer-point buffer)))))

(defadvice (goto-line-command lw-plugins :around) (p)
  (if *vertical-prompt-mode*
    (let* ((buffer (current-buffer))
           (candidates (make-array 10 :fill-pointer 0 :adjustable t))
           (digits (1+ (floor (log (max 1 (editor::buffer-newlines-count buffer)) 10)))))
      (with-buffer-locked (buffer :for-modification nil)
        (with-point ((point (buffers-start buffer))
                     (end (buffers-start buffer)))
          (loop for r = t then (line-offset point 1 0)
                while r
                do (move-point end point)
                   (line-end end)
                   (let ((str (points-to-string point end))
                         (num-str (princ-to-string (1+ (count-lines (buffers-start buffer) point)))))
                     (setq num-str (string-append num-str (make-string (1+ (- digits (length num-str)))
                                                                       :initial-element #\Space)))
                     (vector-push-extend
                      (make-candidate
                       :string (string-append num-str " " str)
                       :display-string
                       (editor::concatenate-buffer-strings
                        (editor::make-buffer-string
                         :%string num-str
                         :properties `((0 ,(length num-str) (editor:face editor::default))))
                        (editor::make-buffer-string
                         :%string str
                         :properties (editor::bounded-text-properties-in point end :modification nil))))
                      candidates)))))
      (let ((selected (vertical-parse :prompt "Goto Line: "
                                      :candidates candidates
                                      :verify-func (lambda (str inf) (declare (ignore inf)) str))))
        (goto-line buffer (parse-integer selected :junk-allowed t))
        (skip-whitespace (buffer-point buffer))))
    (call-next-advice p)))

(bind-key "Goto Line" #("M-g" "g"))
(bind-key "Goto Line" #("M-g" "M-g"))
(bind-key "Goto Definition" #("M-g" "d"))

;; Yank from Kill Ring

(make-face 'yank-from-kill-ring-newline-face
           :foreground :white
           :background :red
           :if-exists :overwrite)

(defun merge-properties (properties)
  (let ((prop-start 0)
        (prop-end 0)
        prop
        result)
    (dolist (props properties)
      (destructuring-bind (ps pe p) props
        (cond ((null prop)
               (setq prop-start ps
                     prop-end pe
                     prop p))
              ((and (= prop-end ps)
                    (equalp prop p))
               (setq prop-end pe))
              (t (push-end (list prop-start prop-end prop) result)
                 (setq prop-start ps
                       prop-end pe
                       prop p)))))
    (push-end (list prop-start prop-end prop) result)
    result))

(defcommand "Yank From Kill Ring" (p)
     "Select a stretch of previously killed text and insert (\"paste\") it."
     "This function will not behave in any reasonable fashion when
  called as a lisp function."
  (declare (ignore p))
  (flet ((props-at (index buffer-string)
           (when (editor::buffer-string-p buffer-string)
             (let ((props (editor::buffer-string-properties buffer-string)))
               (loop for (start end prop) in props
                     do (cond ((<= start index (1- end)) (return prop))
                              ((< index start) (return))))))))
    (if-let (candidates
             (loop for i from 0 below (ring-length editor::*kill-ring*)
                   for item = (ring-ref editor::*kill-ring* i)
                   for string = (editor::buffer-string-string item)
                   for display-string =
                     (loop with new-str = (make-array 15 :element-type 'character :fill-pointer 0 :adjustable t)
                           with props = nil
                           with new-index = 0
                           for index from 0
                           for c across string
                           for prev-whitespace-p = t then whitespace-p
                           for whitespace-p = (whitespace-char-p c)
                           do (cond ((member c '(#\Return #\Newline))
                                     (vector-push-extend #\⏎ new-str)
                                     (push-end (list new-index (1+ new-index) '(editor:face 'yank-from-kill-ring-newline-face)) props)
                                     (incf new-index))
                                    ((and prev-whitespace-p whitespace-p) nil)
                                    (t (vector-push-extend c new-str)
                                       (push-end (list new-index (1+ new-index) (or (props-at index item)
                                                                            '(editor:face editor::default)))
                                                 props)
                                       (incf new-index)))
                           finally (return (editor::make-buffer-string :%string new-str
                                                                       :properties (merge-properties props))))
                   collect (make-candidate :string string :display-string display-string)))
        (insert-string
         (current-point)
         (vertical-parse :prompt "Yank from kill-ring: "
                         :candidates candidates
                         :verify-func (lambda (str inf) (declare (ignore inf)) str)))
      (editor-error "Kill ring is empty"))))

(defadvice (rotate-kill-ring-command lw-plugins :around) (p)
  (if (not (eq (editor::last-command-type) :unkill))
    (yank-from-kill-ring-command p)
    (call-next-advice p)))
