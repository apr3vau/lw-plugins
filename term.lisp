;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

;; A relatively complete terminal feature for LispWorks based on LW
;; Editor

;; This file includes three parts:
;; 1. A stream connected to an Editor buffer, called
;;    ESCAPED-EDITOR-STREAM, which support XTerm-style ANSI escape
;;    sequences
;; 2. A real PTY stream called PTY-STREAM, which will open a
;;    pseudo-tty process on Unix and connects to it;
;; 3. A sample CAPI:EDITOR subclass called PTY-PANE, which combines
;;    these two features and an input handling system into a working
;;    terminal

;; Features:
;; - 4-bit, 8-bit and 24-bit color render
;; - ISO8859-1 and UTF-8 support

;; TODOs
;; - More control sequences
;; - Mouse support (?)
;; - Constraint the Editor pane to fit the terminal better
;; - Optimization
;; - More tests

(defpackage lw-term
  (:add-use-defaults))
(in-package :lw-term)

;; Color definitions

(defvar *term-program* (or (environment-variable "SHELL") "/bin/sh")
  "Default shell program for PTY-PANE. Defaults to $SHELL.")

(defvar *4-bit-colors* (make-array 98)
  "4-bit colors for terminal.

Index 30-37 are normal colors, and 90-97 are bright colors. Indexed
with CSI foreground code, shift -10 to get background.")

(loop for i from 30
      for spec in (list (color:make-rgb 0   0   0)
                        (color:make-rgb 2/3 0   0)
                        (color:make-rgb 0   2/3 0)
                        (color:make-rgb 2/3 1/3 0)
                        (color:make-rgb 0   0   2/3)
                        (color:make-rgb 2/3 0   2/3)
                        (color:make-rgb 0   2/3 2/3)
                        (color:make-rgb 2/3 2/3 2/3))
      do (setf (aref *4-bit-colors* i) spec))

(loop for i from 90
      for spec in (list (color:make-rgb 1/3 1/3 1/3)
                        (color:make-rgb 1   1/3 1/3)
                        (color:make-rgb 1/3 1   1/3)
                        (color:make-rgb 1   1   1/3)
                        (color:make-rgb 1/3 1/3 1)
                        (color:make-rgb 1   1/3 1)
                        (color:make-rgb 1/3 1   1)
                        (color:make-rgb 1   1   1))
      do (setf (aref *4-bit-colors* i) spec))

(defvar *8-bit-colors* (make-array 256)
  "8-bit (256) colors for terminal.")

(loop for i from 0
      for spec in (list (color:make-rgb 0   0   0)
                        (color:make-rgb 2/3 0   0)
                        (color:make-rgb 0   2/3 0)
                        (color:make-rgb 2/3 1/3 0)
                        (color:make-rgb 0   0   2/3)
                        (color:make-rgb 2/3 0   2/3)
                        (color:make-rgb 0   2/3 2/3)
                        (color:make-rgb 2/3 2/3 2/3)
                        (color:make-rgb 1/3 1/3 1/3)
                        (color:make-rgb 1   1/3 1/3)
                        (color:make-rgb 1/3 1   1/3)
                        (color:make-rgb 1   1   1/3)
                        (color:make-rgb 1/3 1/3 1)
                        (color:make-rgb 1   1/3 1)
                        (color:make-rgb 1/3 1   1)
                        (color:make-rgb 1   1   1))
      do (setf (aref *8-bit-colors* i) spec))

(dotimes (r 6)
  (dotimes (g 6)
    (dotimes (b 6)
      (let ((i (+ (* 36 r) (* 6 g) b 16))
            (spec (color:make-rgb (/ r 5) (/ g 5) (/ b 5))))
        (setf (aref *8-bit-colors* i) spec)))))

(loop for i from 232
      for level from 1/32 to 119/128 by 5/128
      do (setf (aref *8-bit-colors* i) (color:make-gray level)))

;; Helper functions

(defun count-lines-after (point)
  "Count how many Newline characters there are after POINT."
  (editor:count-lines point (editor:buffers-end (editor:point-buffer point))))

(defun char-digit (char)
  "Convert digit character to fixnum."
  (declare (inline char-digit))
  (- (char-code char) 48))

(defun char-st-p (char)
  "Check if character is one of terminal ST character"
  (declare (inline char-st-p))
  (member (char-code char) '(#x9C #x1B #x5C)))

(defun ensure-move-to-column (point column)
  "Move POINT to COLUMN, if current line is shorter than COLUMN,
insert space at the end of the line."
  (unless (editor::move-to-column point column)
    (editor:line-end point)
    (let ((count (- column (editor:point-column point))))
      (editor::insert-spaces point count)
      (editor:character-offset point count))))

(defun ensure-line-offset (point offset)
  "Move POINT to OFFSET lines relative to current location. if the
buffer is has shorter line than required, extend buffer at
corresponding location."
  (if (plusp offset)
    (loop repeat offset
          do (unless (editor:line-offset point 1)
               (editor:line-end point)
               (editor:insert-character point #\Newline)
               (editor::point-after point)))
    (loop repeat offset
          do (unless (editor:line-offset point -1)
               (editor:line-start point)
               (editor:insert-character point #\Newline)))))

;; Escaped Editor Stream

;; References:
;; http://www.xfree86.org/4.7.0/ctlseqs.html
;; https://terminalguide.namepad.de

(defclass escaped-editor-stream
          (stream:fundamental-character-output-stream)
  ((buffer :initarg :buffer
           :documentation "The Editor buffer bounded to the stream.")
   (pt :documentation "EDITOR:POINT with :KIND :BEFORE-INSERT that represents current cursor of the terminal.")
   (face :initform (editor:make-face nil)
         :documentation "EDITOR:FACE representing current display attributes. Can be modified by SGR sequences.")
   (pending-sequence :initform nil
                     :documentation "Cached incomplete escaped sequence.")
   (width :initform 80 :initarg :width
          :documentation "Width of the terminal.")
   (height :initform 24 :initarg :height
           :documentation "Height of the terminal.")
   (cursor-visible :initform t))
  (:documentation
   "An output stream connects to an Editor buffer which can handle ANSI excaped sequence."))

(export 'escaped-editor-stream)

(defmethod stream-element-type ((stream escaped-editor-stream))
  'character)

(defmethod initialize-instance :around ((stream escaped-editor-stream) &key)
  "Set the cursor, extend the buffer to the height of the terminal."
  (call-next-method)
  (with-slots (pt buffer height) stream
    ;(editor::set-buffer-contents buffer (make-string height :initial-element #\Newline))
    (setf pt (editor:copy-point (editor:buffer-point buffer) :before-insert))
    (editor:insert-string pt (make-string (1- height) :initial-element #\Newline))
    ))

(defmacro insert-char-with-props (point char props)
  "Insert a character with property PROPS, overwrite if possible.

Used inside the ESCAPED-EDITOR-STREAM."
  `(let ((str (editor::make-buffer-string
               :%string (string ,char)
               :properties `((0 1 ,,props))))
         (c (editor:character-at ,point 0)))
     (if (and c (not (eql c #\Newline)))
       (editor::big-replace-string ,point str 1)
       (editor::insert-buffer-string ,point str))
     (editor::point-after ,point)))

(defun escaped-stream-set-cursor (stream)
  "Set the Editor buffer point to the position of terminal cursor.
If CURSOR-VISIBLE is NIL, suspend this behaviour."
  ;; We noticed that TUI programs will set cursor not visible before
  ;; do a lot of drawing. That's very smart, as from the profiling
  ;; result, moving visible cursor is costly compared with other
  ;; actions.
  
  ;; And there seems no way to "hide" the buffer point in LW Editor...
  (with-slots (buffer pt cursor-visible) stream
    (when cursor-visible
      (when-let (window (first (editor:buffer-windows buffer)))
        (editor:process-character
         (lambda (p)
           (declare (ignore p))
           (when (editor::window-alive-p window)
             (editor:move-point (editor:buffer-point buffer) pt)))
         window)))))

(defun escaped-stream-insert-char (stream char)
  "Insert a character to the Editor buffer associated with STREAM."
  (with-slots (face width) stream
    (let ((pt (slot-value stream 'pt)))
      (case char
        (#\Bell (capi:beep-pane))
        (#\Backspace (editor::point-before pt))
        (#\Tab
         (let* ((col (editor:point-column pt))
                (tab-spaces (editor:variable-value 'editor:spaces-for-tab :global))
                (space-count (mod col tab-spaces)))
           (when (zerop space-count)
             (setq space-count tab-spaces))
           (insert-char-with-props pt #\Tab `(editor:face ,face))
           (editor:with-point ((end pt))
             (editor:line-end end)
             (editor::delete-characters
              pt
              (min space-count (- (editor:point-column end) col))))))
        (#\Newline
         (let ((col (editor:point-column pt)))
           (ensure-line-offset pt 1)
           (ensure-move-to-column pt col)))
        (#\Return
         (editor:line-start pt))
        (#\Rubout
         (if (= (editor:point-column pt) 0)
           (when (< (count-lines-after pt) 80)
             (editor::point-before pt)
             (ensure-move-to-column pt (1- width)))
           (editor::delete-characters pt -1)))
        (#\Escape
         (insert-char-with-props pt #\Tab `(editor:face ,face)))
        (t (insert-char-with-props pt char `(editor:face ,face))))
      (escaped-stream-set-cursor stream))))

(defun escaped-stream-set-face (stream key val)
  "Modify an aspect of the current face of STREAM."
  (let* ((face (slot-value stream 'face))
         (props (list :bold-p      (editor::face-bold-p face)
                      :italic-p    (editor::face-italic-p face)
                      :underline-p (editor::face-underline-p face)
                      :inverse-p   (editor::face-inverse-p face)
                      :foreground  (editor::face-foreground face)
                      :background  (editor::face-background face)
                      :font        (editor::face-font face))))
    (setf (getf props key) val
          (slot-value stream 'face) (apply #'editor:make-face nil props))))

(defun escaped-stream-handle-sgr (stream params)
  "Handle SGR control sequences."
  (prog (param1)
    start
    (setq param1 (or (pop params) 0))
    (case param1
      (0 (setf (slot-value stream 'face) (editor:make-face nil)))
      (1 (escaped-stream-set-face stream :bold-p t))
      (3 (escaped-stream-set-face stream :italic-p t))
      (4 (escaped-stream-set-face stream :underline-p t))
      (7 (escaped-stream-set-face stream :inverse-p t))
      (27 (escaped-stream-set-face stream :inverse-p nil))
      (22 (escaped-stream-set-face stream :bold-p nil))
      (23 (escaped-stream-set-face stream :italic-p nil))
      (24 (escaped-stream-set-face stream :underline-p nil))
      ;; Colors
      ((or 38 48) (case (pop params)
                    (5 (go 8-bit))
                    (2 (go 24-bit))))
      (39 (escaped-stream-set-face stream :foreground nil))
      (49 (escaped-stream-set-face stream :background nil))
      ;; 4-bit
      (t (cond ((or (<= 30 param1 37) (<= 90 param1 97))
                (escaped-stream-set-face stream :foreground (aref *4-bit-colors* param1)))
               ((or (<= 40 param1 47) (<= 100 param1 107))
                (escaped-stream-set-face stream :background (aref *4-bit-colors* (- param1 10)))))))
    (when params (go start))
    (return)
    8-bit
    (escaped-stream-set-face stream (if (= param1 38) :foreground :background)
                             (aref *8-bit-colors* (pop params)))
    (if params (go start) (return))
    24-bit
    (escaped-stream-set-face stream (if (= param1 38) :foreground :background)
                             (destructuring-bind (r g b) params
                               (color:make-rgb (/ r 255) (/ g 255) (/ b 255))))
    (setq params (cdddr params))
    (if params (go start) (return))))

(defun parse-csi-params (chars)
  "Parse an array of characters to CSI parameters (merge digit
characters into integers, mostly)."
  (loop with num
        for index fixnum from 2 to (1- (length chars))
        for char character = (char chars index)
        if (digit-char-p char)
          if num do (setq num (+ (* num 10) (char-digit char)))
          else do (setq num (char-digit char))
        else
          if (<= #x40 (char-code char) #x7E)
            collect num and
            do (loop-finish)
          else
            collect num and
            do (setq num nil) and
            unless (eql char #\;) collect char))

(defun escaped-stream-handle-csi (stream)
  "Handle CSI control sequences."
  (with-slots (pending-sequence face width height buffer cursor-visible) stream
    (let* ((params (parse-csi-params pending-sequence))
           (param1 (first params))
           (pt (slot-value stream 'pt)))
      (labels ((erase-line-before (pt)
                 (editor:with-point ((start pt))
                   (let ((col (editor:point-column pt)))
                     (editor::big-replace-string
                      start
                      (editor::make-buffer-string :%string (make-string col :initial-element #\Space)
                                                  :properties `((0 ,col (editor:face ,face))))
                      col))))
               (erase-line-after (pt)
                 (editor:with-point ((end pt))
                   (editor:line-end end)
                   (editor:delete-between-points pt end)
                   (let ((count (- width (editor:point-column pt))))
                     (editor::insert-buffer-string
                      pt
                      (editor::make-buffer-string :%string (make-string count :initial-element #\Space)
                                                  :properties `((0 ,count (editor:face ,face))))))))
               (erase-line (pt)
                 (editor:with-point ((end pt))
                   (editor:line-end end)
                   (let ((col (editor:point-column pt)))
                     (editor:line-start pt)
                     (editor:delete-between-points pt end)
                     (editor::insert-buffer-string
                      pt
                      (editor::make-buffer-string :%string (make-string width :initial-element #\Space)
                                                  :properties `((0 ,width (editor:face ,face)))))
                     (editor::move-to-column pt col)))))
        (case (char pending-sequence (1- (length pending-sequence)))
          (#\Null ;ICH
           (editor::insert-spaces pt (if (and param1 (plusp param1)) param1 1)))
          (#\A ;CUU
           (let ((col (editor:point-column pt)))
             (loop repeat (if (and param1 (plusp param1)) param1 1)
                   until (or (>= (count-lines-after pt) height)
                             (null (editor:line-offset pt -1))))
             (ensure-move-to-column pt col)))
          (#\B ;CUD
           (let ((col (editor:point-column pt)))
             (loop repeat (if (and param1 (plusp param1)) param1 1)
                   while (editor:line-offset pt 1))
             (ensure-move-to-column pt col)))
          (#\C ;CUF
           (loop repeat (if (and param1 (plusp param1)) param1 1)
                 until (>= (editor:point-column pt) width)
                 if (eql (editor:character-at pt 0) #\Newline)
                   do (editor:insert-character pt #\Space)
                 else do (editor::point-after pt)))
          (#\D ;CUB
           (loop repeat (if (and param1 (plusp param1)) param1 1)
                 until (zerop (editor:point-column pt))
                 do (editor::point-before pt)))
          (#\E ;CNL
           (loop repeat (if (and param1 (plusp param1)) param1 1)
                 while (editor:line-offset pt 1))
           (ensure-move-to-column pt 0))
          (#\F ;CPL
           (loop repeat (if (and param1 (plusp param1)) param1 1)
                 until (>= (count-lines-after pt) (1- height))
                 do (editor:line-offset pt -1))
           (editor::move-to-column pt 0))
          (#\G ;CHA
           (ensure-move-to-column pt (if (and param1 (plusp param1)) param1 1)))
          ((or #\H #\f) ;CUP
           (let ((param2 (second params)))
             (editor:buffer-end pt)
             (unless (editor:line-offset pt (- (if (and param1 (plusp param1)) param1 1) height))
               (editor:buffer-start pt)
               (ensure-line-offset pt (1- height)))
             (ensure-move-to-column pt (1- (if (and param2 (plusp param2)) param2 1)))))
          (#\I ;CHT
           (loop repeat (if (and param1 (plusp param1)) param1 1)
                 do (escaped-stream-insert-char stream #\Tab)))
          (#\J ;ED
           (case (or param1 0)
             (0 (erase-line-after pt)
                (editor:with-point ((sub-pt pt))
                  (loop repeat (count-lines-after sub-pt)
                        do (editor::line-offset sub-pt 1)
                           (erase-line sub-pt))))
             (1 (erase-line-before pt)
                (editor:with-point ((sub-pt pt))
                  (loop repeat (- height 1 (count-lines-after sub-pt))
                        do (editor::line-offset sub-pt -1)
                           (erase-line sub-pt))))
             (t (let ((col (editor:point-position pt))
                      (r (count-lines-after pt)))
                  (editor:buffer-end pt)
                  (loop repeat height
                        do (erase-line pt)
                           (editor:line-offset pt -1 0))
                  (when (= param1 3)
                    (editor:delete-between-points (editor:buffers-start buffer) pt))
                  (editor:buffer-end pt)
                  (editor:line-offset pt (- r) col)))))
          (#\K ;EL
           (case (or param1 0)
             (0 (erase-line-after pt))
             (1 (erase-line-before pt))
             (2 (erase-line pt))))
          (#\L ;IL
           (editor::move-to-column pt 0)
           (let* ((count (if (and param1 (plusp param1)) param1 1))
                  (str (apply #'string-append
                              (loop repeat count
                                    collect (make-string width :initial-element #\Space)
                                    collect (string #\Newline)))))
             (editor::insert-buffer-string
              pt (editor::make-buffer-string :%string str
                                             :properties `((0 (length str) (editor:face ,face)))))
             (editor:with-point ((end (editor:buffers-end buffer))
                                 (start (editor:buffers-end buffer)))
               (editor:line-offset start (- count) 0)
               (editor:delete-between-points start end))))
          (#\M ;DL
           (editor::move-to-column pt 0)
           (let ((count (if (and param1 (plusp param1)) param1 1)))
             (editor:with-point ((end pt))
               (editor:line-offset end count 0)
               (editor:delete-between-points pt end)
               (editor:buffer-end end)
               (editor:insert-string end (make-string count :initial-element #\Newline)))))
          (#\d ;VPA
           (let ((col (editor:point-column pt)))
             (editor:buffer-end pt)
             (unless (editor:line-offset pt (- (if (and param1 (plusp param1)) param1 1) height))
               (editor:buffer-start pt)
               (ensure-line-offset pt (1- height)))
             (ensure-move-to-column pt col)))
          (#\h ;DECSET
           (if (eql param1 #\?)
             (loop for param in (cdr params)
                   do (case param
                        (25 (setf cursor-visible t))))
             ;; TODO
             ))
          (#\l ;DECRST
           (if (eql param1 #\?)
             (loop for param in (cdr params)
                   do (case param
                        (25 (setf cursor-visible nil))))
             ;; TODO
             ))
          (#\m (escaped-stream-handle-sgr stream params))))
      (setf pending-sequence nil)
      (escaped-stream-set-cursor stream))))

(defun escaped-stream-handle-other-codes (stream)
  "Here we handle ESC escape, some C0 codes, and discard others we don't support..."
  (with-slots (pending-sequence pt) stream
    (cond ((equal pending-sequence "")
           (escaped-stream-insert-char pending-sequence #\Escape)
           (setf pending-sequence nil)
           (escaped-stream-set-cursor stream))
          ((equal pending-sequence "D") ;IND
           (let ((col (editor:point-column pt)))
             (editor:line-offset pt 1)
             (ensure-move-to-column pt col)
             (escaped-stream-set-cursor stream)))
          ((equal pending-sequence "M") ;RI
           (let ((col (editor:point-column pt)))
             (editor:line-offset pt -1)
             (ensure-move-to-column pt col)
             (escaped-stream-set-cursor stream)))
          ;; For #, % and space, their control sequences are length 3,
          ;; so we need to read one more character
          ((and (= (length pending-sequence) 2)
                (member (char pending-sequence 1) '(#\# #\% #\Space)))
           t)
          (t (setf pending-sequence nil)))))

(defmethod stream:stream-write-char ((stream escaped-editor-stream) char)
  (with-slots (pending-sequence) stream
    (if pending-sequence
      (progn
        (vector-push-extend char pending-sequence)
        (case (char pending-sequence 1)
          (#\[ (when (and (> (length pending-sequence) 2)
                          (<= #x40 (char-code char) #x7E))
                 (escaped-stream-handle-csi stream)))
          (t (escaped-stream-handle-other-codes stream))))
      (if (eql char #\Escape)
        (setf pending-sequence (make-array 10 :element-type 'character :initial-element #\Escape :fill-pointer 1 :adjustable t))
        (escaped-stream-insert-char stream char)))))

;; FLI C functions used in PTY-STREAM

(fli:define-foreign-function (forkpty "forkpty")
    ((amaster (:ptr :int))
     (name :ptr)
     (termp :ptr)
     (winp :ptr))
  :result-type :int)

(fli:define-foreign-function (fd-read "read")
    ((fd :int)
     (buf :ptr)
     (nbyte :size-t))
  :result-type :size-t)

(fli:define-foreign-function (fd-write "write")
    ((fd :int)
     (buf :ptr)
     (nbyte :size-t))
  :result-type :size-t)

(fli:define-foreign-function (fd-close "close")
    ((fd :int))
  :result-type :int)

(fli:define-foreign-function (pkill "kill")
    ((pid :int)
     (sig :int))
  :result-type :int)

(fli:define-foreign-function (execve "execve")
    ((path :ptr)
     (args :ptr)
     (envp :ptr))
  :result-type :int)

(defconstant +sigterm+ 15 "SIGTERM enum value")

;; PTY Stream

(defclass pty-stream (stream:fundamental-binary-input-stream
                      stream:fundamental-binary-output-stream
                      stream:fundamental-character-input-stream
                      stream:fundamental-character-output-stream)
  ((pty-process :documentation "The process that starts forks out the PTY hand handling file descriptors.")
   (master-fd :documentation "PTY Master file descriptor.")
   (master-pid :documentation "PTY master process PID returned by forkpty()")
   (command :initform '("/bin/sh")
            :initarg :command
            :documentation "A list of commands that will be executed in the new PTY process with execve()")
   (environment :initform nil
                :initarg :environment
                :documentation "Environment variables for execve()")
   (ctype :initform "UTF-8"
          :initarg :ctype
          :documentation "Stream character type, same with the LC_CTYPE. Only support UTF-8 and latin1 currently."))
  (:documentation "A character & binary IO stream that connects with a real Unix pseudo-tty using forkpty()"))

(defmethod initialize-instance :around ((stream pty-stream) &key)
  "Start the PTY"
  (call-next-method)
  (with-slots (command environment ctype) stream
    (let ((amaster (fli:allocate-foreign-object :type :int)))
      (unwind-protect
          (progn
            (let ((pid (forkpty amaster nil nil nil)))
              (cond ((minusp pid) (error "Failed when executing forkpty()"))
                    ((zerop pid)
                     ;; in new process
                     (let ((args (fli:allocate-foreign-object
                                  :type :ptr 
                                  :initial-contents (mapcar #'fli:convert-to-foreign-string command)))
                           (envp (when environment
                                   (fli:allocate-foreign-object
                                    :type :ptr
                                    :initial-contents (mapcar #'fli:convert-to-foreign-string
                                                              (loop for (name . value) in environment
                                                                    collect (string-append name "=" value)))))))
                       (execve (fli:dereference args :index 0) args envp)))
                    ;; in main process
                    (t (setf (slot-value stream 'master-pid) pid))))
            (setf (slot-value stream 'master-fd) (fli:dereference amaster)))
        (fli:free amaster)))))

(defmethod stream:stream-read-byte ((stream pty-stream))
  "Read one byte from PTY-STREAM using C's read()"
  (loop until (slot-boundp stream 'master-fd)
        do (sleep 0.01))
  (let ((c (fli:allocate-foreign-object :type '(:unsigned :byte))))
    (unwind-protect
        (let ((ret (fd-read (slot-value stream 'master-fd) c 1)))
          (cond ((minusp ret) (error "Failed when executing read()"))
                ((zerop ret) :eof)
                (t (fli:dereference c))))
      (fli:free c))))

(defmethod stream:stream-write-byte ((stream pty-stream) byte)
  "Write one byte from PTY-STREAM using C's write()"
  (loop until (slot-boundp stream 'master-fd)
        do (sleep 0.01))
  (let ((c (fli:allocate-foreign-object :type '(:unsigned :byte) :initial-element byte)))
    (unwind-protect
        (let ((ret (fd-write (slot-value stream 'master-fd) c 1)))
          (cond ((minusp ret) (error "Failed when executing write()"))
                ((zerop ret) :eof)
                (t byte)))
      (fli:free c))))

(defmethod stream:stream-read-char ((stream pty-stream))
  (with-slots (ctype) stream
    (cond
     ((search "UTF-8" ctype :test #'string-equal)
      (let ((first-byte (stream:stream-read-byte stream)))
        (if (not (fixnump first-byte)) :eof
          (let ((sign (floor first-byte 16)))
            ;; Treat #x08 - #x0B as latin-1 supplement, although they're illegal in UTF-8...
            (if (< sign #xC) (code-char first-byte)
              (let ((lst (list first-byte)))
                (when (<= sign #xD)
                  (push-end (stream:stream-read-byte stream) lst))
                (when (<= sign #xE)
                  (push-end (stream:stream-read-byte stream) lst))
                (when (<= sign #xF)
                  (push-end (stream:stream-read-byte stream) lst))
                (let* ((len (length lst))
                       (ptr (fli:allocate-foreign-object :type '(:unsigned :byte) :nelems len :initial-contents lst)))
                  (unwind-protect
                      (char (fli:convert-from-foreign-string ptr :external-format :utf-8 :length len :null-terminated-p nil) 0)
                    (fli:free ptr)))))))))
     (t (code-char (stream:stream-read-byte stream))))))

(defmethod stream:stream-write-char ((stream pty-stream) char)
  (with-slots (ctype) stream
    (let ((code (char-code char)))
      (cond
       ((search "UTF-8" ctype :test #'string-equal)
        (if (< code #x80)
          (stream:stream-write-byte stream code)
          (multiple-value-bind (ptr len)
              (fli:convert-to-foreign-string (string char) :external-format :utf-8 :null-terminated-p nil)
            (dotimes (i len)
              (stream:stream-write-byte stream (fli:dereference ptr :index i))))))
       (t (stream:stream-write-byte stream code))))))

(defmethod close ((stream pty-stream) &key abort)
  (declare (ignore abort))
  (when (minusp (pkill (slot-value stream 'master-pid) +sigterm+))
    (error "Failed when executing kill()"))
  (when (minusp (fd-close (slot-value stream 'master-fd)))
    (error "Failed when executing close()")))

;; PTY Pane

(defun pty-pane-gesture (pane x y spec)
  (declare (ignore x y))
  (with-slots (pty-stream) pane
    (char-code #\Backspace)
    (let* ((data (sys:gesture-spec-data spec))
           (mod (case (sys:gesture-spec-modifiers spec)
                  (0 0) (1 2) (2 5) (3 6)
                  (6 7) (4 9) (5 10) (7 14))))
      (if (fixnump data)
        (let ((char (code-char data)))
          (when (eql char #\Backspace)
            (setq char #\Rubout))
          (case mod
            (0 (write-char char pty-stream))
            (5 (if (alpha-char-p char)
                 (if (upper-case-p char)
                   (write-byte (- data 64) pty-stream)
                   (write-byte (- data 96) pty-stream))
                 (case char
                   (#\@ (write-char #\Null pty-stream))
                   (#\[ (write-char #\Escape pty-stream))
                   (#\\ (write-char #\FS pty-stream))
                   (#\] (write-char #\GS pty-stream))
                   (#\6 (write-char #\RS pty-stream))
                   (#\- (write-char #\US pty-stream))
                   (#\/ (write-char #\US pty-stream)))))
            (t (format pty-stream "[~A;~A~~" mod data))))
        (format pty-stream "[~A~C" (if (zerop mod) "" mod)
                (case data
                  (:up #\A) (:down #\B) (:right #\C) (:left #\D)
                  (:home #\H) (:end #\F)
                  (:f1 #\P) (:f2 #\Q) (:f3 #\R) (:f4 #\S)))))
    (format nil "~16R" (char-code #\Escape))
    (force-output pty-stream)))

(defclass pty-pane (capi:editor-pane)
  ((pty-stream :initform nil)
   (escaped-output-stream :initform nil)
   (relay-process :initform nil))
  (:default-initargs
   :buffer :temp
   :visible-min-width '(character 83)
   :visible-min-height '(character 24)
   :input-model '((:gesture-spec pty-pane-gesture))
   :create-callback (lambda (pane)
                      (with-slots (pty-stream escaped-output-stream relay-process) pane
                        (let ((buffer (capi:editor-pane-buffer pane)))
                          (setf escaped-output-stream (make-instance 'escaped-editor-stream :buffer buffer)
                                pty-stream (make-instance
                                            'pty-stream
                                            :command (list "/bin/sh" "-c"
                                                           (string-append
                                                            "stty sane rows 24 columns 80 echo icrnl iutf8 > /dev/null; export LC_CTYPE=UTF-8 COLORFGBG=15\\;0 COLORTERM=truecolor; "
                                                            *term-program*
                                                            ))
                                            :environment '(("TERM" . "xterm-256color")
                                                           ("LINES" . "24")
                                                           ("COLUMNS" . "80")))
                                relay-process (mp:process-run-function
                                               "Term Relay" ()
                                               (lambda ()
                                                 (loop for char = (read-char pty-stream nil)
                                                       if char do (write-char char escaped-output-stream)
                                                       else do (return))))))))
   :destroy-callback (lambda (pane)
                       (with-slots (relay-process pty-stream escaped-output-stream) pane
                         (close pty-stream)
                         (mp:process-kill relay-process)
                         (close escaped-output-stream)))))

;(capi:contain (make-instance 'pty-pane))
