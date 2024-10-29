;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

;; Lisp form aligning facility.

;; Aligning single list, multiple lists, LET / DEFCLASS / MAKE-INSTANCE form

;; See the documentation of Editor command "Align" for details.

(defpackage align
  (:use #:editor)
  (:add-use-defaults))
(in-package align)

(defmacro do-each-form-start (point start end &body body)
  (let ((pt (gensym "POINT-")))
    `(with-point ((,pt ,start))
       (loop (setq ,pt (form-offset ,pt 1))
             (when (or (null ,pt)
                       (and ,end (point> ,pt ,end)))
               (return))
             (form-offset ,pt -1)
             (with-point ((,point ,pt))
               ,@body)
             (form-offset ,pt 1)))))

(defun collect-forms-length (start &optional end)
  (let (lengths)
    (do-each-form-start pt start end
      (with-point ((end pt))
        (form-offset end 1)
        (let ((str (points-to-string pt end)))
          (push-end (if (find #\Newline str) -1 (length str))
                    lengths))))
    lengths))

(defun collect-subform-length (start)
  (with-point ((start start))
    (loop for c = (character-at start 0)
          while (whitespace-char-p c)
          do (editor::point-after start))
    (when (eql (character-at start 0) #\()
      (collect-forms-length (form-offset start 1 t -1)))))

(defun collect-subforms-lengths (start &optional end)
  (let (lengths)
    (do-each-form-start pt start end
      (push-end (collect-subform-length pt) lengths))
    lengths))

(defun delete-whitespace-before (pt)
  (loop for i downfrom -1
        for c = (character-at pt i)
        while (whitespace-char-p c)
        finally (editor::delete-characters pt (1+ i))))

(defun align-let (start)
  "Align all binding values in LET / LET* form starts at START."
  (let (start-column values-start)
    (with-point ((start start))
      (form-offset start 1 t -2) ; (let ($(a 1) (b 2)))
      (setq start-column (point-column start)
            values-start
            (+ 2 start-column
               (apply #'max (mapcar #'first (collect-subforms-lengths start))))))
    (with-point ((start start))
      (form-offset start 1 t -2)
      (let ((count 0))
        (do-each-form-start pt start nil
          (when (eql (character-at pt 0) #\()
            (unless (= count 0)
              (delete-whitespace-before pt)
              (insert-things pt #\Newline (make-string start-column :initial-element #\Space)))
            (print (point-column pt))
            (when (form-offset pt 3 t -1)
              (form-offset pt -1)
              (let ((offset (- values-start (point-column pt))))
                (cond ((plusp  offset) (editor::insert-spaces pt offset))
                      ((minusp offset) (editor::delete-characters pt offset)))))
            (incf count)))))
    (with-point ((end start))
      (form-offset end 1)
      (funcall (variable-value 'editor::indent-region-function) start end))))

(defun align-make-instance (start)
  "Align MAKE-INSTANCE form starts at START."
  (let (start-column values-start)
    (with-point ((start start))
      (form-offset start 3 t -1)
      (loop while (whitespace-char-p (character-at start 0))
            do (editor::point-after start))
      (setq start-column (point-column start)
            values-start (+ 1 start-column
                            (loop for (kw-len nil) on (collect-forms-length start) by #'cddr
                                  maximize kw-len))))
    (with-point ((start start))
      (let ((count 1))
        (form-offset start 3 t -1)
        (loop while (whitespace-char-p (character-at start 0))
              do (editor::point-after start))
        (do-each-form-start pt start nil
          (if (oddp count)
              (progn
                (delete-whitespace-before pt)
                (insert-things pt #\Newline (make-string start-column :initial-element #\Space)))
            (let ((offset (- values-start (point-column pt))))
              (cond ((plusp  offset) (editor::insert-spaces pt offset))
                    ((minusp offset) (editor::delete-characters pt offset)))))
          (incf count))))
    (with-point ((end start))
      (form-offset end 1)
      (funcall (variable-value 'editor::indent-region-function) start end))))

(defun align-defclass (start)
  "Align DEFCLASS form starts at START.

Align all pairs of keywords and values in slot definitions, and pairs
of keywords & values in :DEFAULT-INITARGS"
  (let ((keywords-start 0) (keywords-max-length 0)
        start-column values-start)
    (with-point ((slot-defs-start start))
      (form-offset slot-defs-start 4 t -1) ; (defclass class ()$
      (form-offset slot-defs-start 1 t -1) ;    ($(a :initform nil)))
      
      (do-each-form-start pt slot-defs-start nil
        (let ((count 1))
          (form-offset pt 1 t -1)
          (do-each-form-start sub-pt pt nil
            (when (evenp count)
              (with-point ((end sub-pt))
                (form-offset end 1)
                (setq keywords-start (max keywords-start (point-column sub-pt))
                      keywords-max-length (max keywords-max-length (length (points-to-string sub-pt end))))))
            (incf count))))
      
      (setq values-start (+ 1 keywords-start keywords-max-length))
      
      (do-each-form-start pt slot-defs-start nil
        (let ((count 1))
          (form-offset pt 1 t -1)
          (do-each-form-start sub-pt pt nil
            (if (evenp count)
                (if (= count 2)
                    (let ((offset (- keywords-start (point-column sub-pt))))
                      (cond ((plusp  offset) (editor::insert-spaces sub-pt offset))
                            ((minusp offset) (editor::delete-characters sub-pt offset))))
                  (progn
                    (delete-whitespace-before sub-pt)
                    (insert-things sub-pt #\Newline (make-string keywords-start :initial-element #\Space))))
              (unless (= count 1)
                (delete-whitespace-before sub-pt)
                (let ((offset (- values-start (point-column sub-pt))))
                  (cond ((plusp offset)  (editor::insert-spaces sub-pt offset))
                        ((minusp offset) (editor::delete-characters sub-pt offset))))))
            (incf count)))))
    
    (block default-initargs
      (with-point ((default-initargs-start start))
        (form-offset default-initargs-start 5 t -1)
        (do-each-form-start pt default-initargs-start nil
          (when (eql (character-at pt 0) #\()
            (editor::point-after pt)
            (when (eq (get-symbol-from-point pt :create-new nil)
                      :default-initargs)
              (move-point default-initargs-start pt)
              (return)))) ;; ($:default-initargs ...)
        (setq default-initargs-start (form-offset default-initargs-start 2))
        (unless default-initargs-start
          (return-from default-initargs))
        (form-offset default-initargs-start -1)
        (setq start-column (point-column default-initargs-start)
              keywords-max-length (apply #'max (loop for (len nil) on (collect-forms-length default-initargs-start)
                                                       by #'cddr
                                                     collect len))
              values-start (+ 1 start-column keywords-max-length))
        (print (list start-column values-start))
        (let ((count 1))
          (do-each-form-start pt default-initargs-start nil
            (if (oddp count)
                (progn
                  (delete-whitespace-before pt)
                  (insert-things pt #\Newline (make-string start-column :initial-element #\Space)))
              (progn
                (delete-whitespace-before pt)
                (let ((offset (- values-start (point-column pt))))
                  (cond ((plusp  offset) (editor::insert-spaces pt offset))
                        ((minusp offset) (editor::delete-characters pt offset))))))
            (incf count)))))
  (with-point ((end start))
    (form-offset end 1)
    (funcall (variable-value 'editor::indent-region-function) start end))))

(defun align-list (start &optional lambda-list-p)
  "Align list starts at START.

Number of forms each line is determined by the first line.

If LAMBDA-LIST-P it T, escape the first form of the list."
  (let (start-column columns (columns-count 0))
    (with-point ((start start)
                 (end start))
      (form-offset end 1)
      (form-offset start 1 t -1)
      ;; Count forms at first line, use it as maximum forms each line
      (when lambda-list-p
        (form-offset start 1)
        (loop while (whitespace-char-p (character-at start 0))
              do (editor::point-after start)))
      (with-point ((pt start))
        (setq start-column (point-column pt))
        (with-point ((pt pt))
          (let (last-pt)
            (loop (setq last-pt (copy-point pt :temporary)
                        pt (form-offset pt 1))
                  (when (or (null pt)
                            (find #\Newline (points-to-string last-pt pt)))
                    (return))
                  (incf columns-count)))))
      (setq columns (make-list columns-count :initial-element 0))
      ;; Find the maximum length for forms in each column
      (let ((count 0))
        (do-each-form-start pt start nil
          (with-point ((form-end pt))
            (form-offset form-end 1)
            (setf (nth count columns)
                  (max (nth count columns)
                       (length (points-to-string pt form-end)))))
          (incf count)
          (when (= count columns-count)
            (setq count 0))))
      ;; For each symbol, redistribute whitespace
      (with-point ((pt start))
        (let ((count 0) prev-pt)
          (loop (loop for i from 0
                      for c = (character-at pt i)
                      until (not (whitespace-char-p c))
                      finally (editor::delete-characters pt i))
                (if (= count columns-count)
                    (unless (eql (character-at pt 0) #\))
                      (insert-things pt #\Newline (make-string start-column :initial-element #\Space))
                      (setq count 0))
                  (let* ((prev   (subseq columns 0 count))
                         (target (+ start-column (apply #'+ (or prev '(0))) (length prev)))
                         (offset (- target (point-column pt))))
                    (cond ((plusp offset)  (editor::insert-spaces pt offset))
                          ((minusp offset) (editor::delete-characters pt offset)))))
                (incf count)
                (setq prev-pt (copy-point pt :temporary)
                      pt (form-offset pt 1))
                (when (or (null pt) (point>= pt end))
                  (return))
                ;; When there's multi-line form, re-indent all lines in it
                (when (> (count #\Newline (points-to-string prev-pt pt)) 1)
                  (funcall (variable-value 'editor::indent-region-function) prev-pt pt))))))))

(defun align-lists (start end)
  "Align the content of a list of lists between START and END."
  (let (end-line item-start-column columns)
    (with-point ((pt start))
      (form-offset pt 1 t -1)
      (setq item-start-column (point-column pt))
      (buffer-start pt)
      (setq end-line (count-lines pt end)))
    (with-point ((pt start))
      (loop
         (block current-line
           (let ((count 0) form-end)
             (with-point ((sub-pt pt))
               (setq sub-pt (form-offset sub-pt 1 t -1))
               (when (null sub-pt) (return-from current-line))
               (loop (setq sub-pt (form-offset sub-pt 1))
                     (when (null sub-pt)
                       (return-from current-line))
                     (setq form-end (copy-point sub-pt))
                     (form-offset sub-pt -1)
                     (when (<= (length columns) count)
                       (push-end 0 columns))
                     (setf (nth count columns)
                           (max (nth count columns)
                                (length (points-to-string sub-pt form-end))))
                     (move-point sub-pt form-end)
                     (incf count)))))
         (setq pt (form-offset pt 2))
         (when (null pt) (return))
         (setq pt (form-offset pt -1))
         (when (point>= pt end) (return))))
    (with-point ((pt start))
      (loop (block current-form
              (with-point ((sub-pt pt))
                (setq sub-pt (form-offset sub-pt 2 t -1))
                (when (null sub-pt) (return))
                (let ((line-start 0)
                      (last-pt    (copy-point sub-pt :temporary)))
                  (loop for i from 1
                        do (when (null sub-pt) (return-from current-form))
                           (when (find #\Newline (points-to-string last-pt sub-pt))
                             (setq line-start (1- i)))
                           (editor::delete-horizontal-space sub-pt)
                           (if (eql (character-at sub-pt 0) #\))
                               (return-from current-form)
                             (let* ((prev   (subseq columns line-start i))
                                    (target (+ item-start-column (apply #'+ (or prev '(0))) (length prev)))
                                    (offset (- target (point-column sub-pt))))
                               (cond ((plusp offset)  (editor::insert-spaces sub-pt offset))
                                     ((minusp offset) (editor::delete-characters sub-pt offset)))))
                           (setq last-pt (copy-point sub-pt :temporary)
                                 sub-pt (form-offset sub-pt 1))))))
            (setq pt (form-offset pt 1))
            (when (null pt) (return))
            (with-point ((buffer-start pt))
              (buffer-start buffer-start)
              (when (>= (count-lines buffer-start pt) end-line)
                (return)))))))

(defcommand "Align" (p)
     "Align the current form.

Select a list of lists, will align their contents;

Place cursor in / at-start-of a list, align its content based on the
number of forms at first line; if the first symbol of the list is a
function, it will be excluded from aligning;

Specially, place cursor in / at-start-of a LET / LET* / DEFCLASS /
MAKE-INSTANCE form, will align their contents in correct way."
     "Use ALIGN-* functions with the POINT at the list's start you
desire to align, to align specific form."
  (declare (ignore p))
  (let* ((buffer        (current-buffer))
         (highlighted-p (buffer-region-highlighted-p buffer))
         (point         (buffer-point buffer))
         (mark          (or (buffer-mark buffer t) point)))
    (collect-undo buffer
      (handler-case
          (if (and highlighted-p
                   (if (point> mark point)
                       (point< (form-offset (copy-point point :temporary) 1)
                               mark)
                     (point< (form-offset (copy-point mark :temporary) 1)
                             point)))
              (align-lists (if (point< mark point) mark point)
                           (if (point> point mark) point mark))
            (with-point ((point point))
              (cond ((eql (character-at point 0) #\() (editor::point-after point))
                    ((null (form-offset (copy-point point :temporary) -1 t 1)) t)
                    (t (loop for r = (form-offset point -1) until (null r))))
              (let ((sym (if (eql (character-at point 0) #\() nil
                           (get-symbol-from-point point :create-new nil))))
                (editor::point-before point)
                (cond ((eq sym 'defclass)       (align-defclass      point))
                      ((member sym '(let let*)) (align-let           point))
                      ((eq sym 'make-instance)  (align-make-instance point))
                      ((fboundp sym)            (align-list          point t))
                      (t                        (align-list          point))))))
        (error (e) (editor-error "Error while aligning: ~A" e))))))

