;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

;; Lisp form aligning facility.

;; Aligning single list, multiple lists, LET / DEFCLASS / MAKE-INSTANCE form

;; See the documentation of Editor command "Align" for details.

(in-package editor)

(defun align-let (start)
  "Align all binding values in LET / LET* form starts at START."                                                                                         
  (let ((target-value-start 0))
    (with-point ((pt start))
      (form-offset pt 1 t -2) ; (let ($(a 1) (b 2)))
      (loop (with-point ((sub-pt pt))
              (setq sub-pt (form-offset sub-pt 1))
              (when (null sub-pt) (return))
              (move-point pt sub-pt)
              (when (eql (character-at sub-pt -1) #\))
                (form-offset sub-pt -1)
                (form-offset sub-pt 1 t -1)
                (setf target-value-start
                      (max target-value-start
                           (point-column (form-offset (form-offset sub-pt 2) -1))))))))
    (with-point ((pt start))
      (form-offset pt 1 t -2)
      (loop (with-point ((sub-pt pt))
              (setq sub-pt (form-offset sub-pt 1))
              (when (null sub-pt) (return))
              (move-point pt sub-pt)
              (when (eql (character-at sub-pt -1) #\))
                (form-offset sub-pt -1)
                (form-offset sub-pt 1 t -1)
                (form-offset sub-pt 2)
                (form-offset sub-pt -1)
                (let ((offset (- target-value-start (point-column sub-pt))))
                  (cond ((plusp offset) (insert-spaces sub-pt offset))
                        ((minusp offset) (delete-characters sub-pt offset))))))))
    (with-point ((end start))
      (form-offset end 1)
      (indent-region-for-commands start end nil))))

(defun align-make-instance (start)
  "Align MAKE-INSTANCE form starts at START."                                                                                                            
  (let (start-column (keyword-max-length 0))
    (with-point ((pt start))
      (form-offset pt 3 t -1)
      (loop while (whitespace-char-p (character-at pt 0))
            do (point-after pt))
      (setq start-column (point-column pt))
      (let (keyword-start)
        (loop (loop while (whitespace-char-p (character-at pt 0))
                    do (point-after pt))
              (setq keyword-start (copy-point pt :temporary))
              (setq pt (form-offset pt 1))
              (when (null pt) (return))
              (setq keyword-max-length
                    (max keyword-max-length
                         (length (points-to-string keyword-start pt))))
              (setq pt (form-offset pt 1))
              (when (null pt) (return)))))
    (with-point ((pt start))
      (let ((target-value-start (+ 1 start-column keyword-max-length)))
        (form-offset pt 3 t -1)
        (loop while (whitespace-char-p (character-at pt 0))
              do (point-after pt))
        (loop (setq pt (form-offset pt 1))
              (when (null pt) (return))
              (let ((offset (- target-value-start (point-column pt))))
                (cond ((plusp offset)  (insert-spaces pt offset))
                      ((minusp offset) (delete-characters pt offset))))
              (setq pt (form-offset pt 1))
              (when (null pt) (return))
              (loop for i from 0
                    for c = (character-at pt i)
                    until (not (whitespace-char-p c))
                    finally (delete-characters pt i))
              (when (eql (character-at pt 0) #\)) (return))
              (insert-things pt #\Newline (make-string start-column :initial-element #\Space)))))
    (with-point ((end start))
      (form-offset end 1)
      (indent-region-for-commands start end nil))))

(defun align-defclass (start)
  "Align DEFCLASS form starts at START.

Align all pairs of keywords and values in slot definitions, and pairs
of keywords & values in :DEFAULT-INITARGS"                                                                                                               
  (let ((target-keyword-start 0) (keyword-max-length 0))
    (with-point ((pt start))
      (form-offset pt 4 t -1) ; (defclass class ()$
      (form-offset pt 1 t -1) ;    ($(a :initform nil))
      (loop (with-point ((sub-pt pt))
              (setq sub-pt (form-offset sub-pt 1 t -1))
              (when (null sub-pt) (return))
              (form-offset sub-pt 1)
              (loop (setq sub-pt (form-offset sub-pt 1))
                    (if sub-pt
                        (let ((keyword-end (copy-point sub-pt :temporary))
                              (keyword-start (copy-point (form-offset sub-pt -1) :temporary)))
                          (setq target-keyword-start
                                (max target-keyword-start
                                     (point-column keyword-start))
                                keyword-max-length
                                (max keyword-max-length
                                     (length (points-to-string keyword-start keyword-end))))
                          (form-offset sub-pt 2))
                      (return)))
              (form-offset pt 1))))
    (with-point ((pt start))
      (form-offset pt 4 t -1)
      (form-offset pt 1 t -1)
      (let ((target-value-start (+ 1 target-keyword-start keyword-max-length)))
        (loop (with-point ((sub-pt pt))
                (setq sub-pt (form-offset sub-pt 1 t -1))
                (when (null sub-pt) (return))
                (form-offset sub-pt 1)
                (loop (setq sub-pt (form-offset sub-pt 1))
                      (if sub-pt
                          (progn
                            (form-offset sub-pt -1)
                            (let ((offset (- target-keyword-start (point-column sub-pt))))
                              (cond ((plusp offset)  (insert-spaces sub-pt offset))
                                    ((minusp offset) (delete-characters sub-pt offset))))
                            (form-offset sub-pt 2)
                            (form-offset sub-pt -1)
                            (let ((offset (- target-value-start (point-column sub-pt))))
                              (cond ((plusp offset)  (insert-spaces sub-pt offset))
                                    ((minusp offset) (delete-characters sub-pt offset))))
                            (form-offset sub-pt 1))
                        (return)))
                (form-offset pt 1))))))
  (block default-initargs
    (let ((keyword-max-length 0) start-column)
      (flet ((delete-whitespace-after (pt)
               (loop for i from 0
                     for c = (character-at pt i)
                     until (not (whitespace-char-p c))
                     finally (delete-characters pt i))))
        (with-point ((pt start))
          (form-offset pt 5 t -1)
          (loop (with-point ((sub-pt pt))
                  (setq sub-pt (form-offset sub-pt 1 t -1))
                  (when (null sub-pt) (return-from default-initargs))
                  (if (eq (buffer-symbol-at-point (point-buffer sub-pt) :point sub-pt)
                          :default-initargs)
                      (progn
                        (move-point pt sub-pt)
                        (return))
                    (form-offset pt 1))))
          (form-offset pt 2)
          (form-offset pt -1)
          (setq start-column (point-column pt))
          (with-point ((pt pt))
            (let (keyword-start keyword-end)
              (loop (setq pt (form-offset pt 1))
                    (when (null pt) (return))
                    (setq keyword-end        (copy-point pt :temporary)
                          keyword-start      (copy-point (form-offset pt -1) :temporary)
                          keyword-max-length (max keyword-max-length
                                                  (length (points-to-string keyword-start keyword-end)))
                          pt                 (form-offset pt 2))
                    (when (null pt) (return)))))
          (let ((values-column (+ 1 start-column keyword-max-length)))
            (form-offset pt 1)
            (loop (setq pt (form-offset pt 1))
                  (when (null pt) (return))
                  (form-offset pt -1)
                  (let ((offset (- values-column (point-column pt))))
                    (cond ((plusp offset)  (insert-spaces pt offset))
                          ((minusp offset) (delete-characters pt offset))))
                  (form-offset pt 1)
                  (delete-whitespace-after pt)
                  (when (eq (character-attribute :lisp-syntax (character-at pt 0)) :close-paren)
                    (return))
                  (insert-things pt #\Newline (make-string start-column :initial-element #\Space))
                  (setq pt (form-offset pt 1))
                  (when (null pt) (return)))))))
    (with-point ((end start))
      (form-offset end 1)
      (funcall (variable-value 'indent-region-function) start end))))

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
              do (point-after start)))
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
      (with-point ((pt start))
        (let ((count 0) form-end)
          (loop (setq pt (form-offset pt 1))
                (when (null pt) (return))
                (setq form-end (copy-point pt :temporary))
                (form-offset pt -1)
                (setf (nth count columns)
                      (max (nth count columns)
                           (length (points-to-string pt form-end))))
                (move-point pt form-end)
                (incf count)
                (when (= count columns-count)
                  (setq count 0)))))
      ;; For each symbol, redistribute whitespace
      (with-point ((pt start))
        (let ((count 0) prev-pt)
          (loop (loop for i from 0
                      for c = (character-at pt i)
                      until (not (whitespace-char-p c))
                      finally (delete-characters pt i))
                (if (= count columns-count)
                    (unless (eq (character-attribute :lisp-syntax (character-at pt 0)) :close-paren)
                      (progn
                        (insert-things pt #\Newline (make-string start-column :initial-element #\Space))
                        (setq count 0)))
                  (let* ((prev   (subseq columns 0 count))
                         (target (+ start-column (apply #'+ (or prev '(0))) (length prev)))
                         (offset (- target (point-column pt))))
                    (cond ((plusp offset)  (insert-spaces pt offset))
                          ((minusp offset) (delete-characters pt offset)))))
                (incf count)
                (setq prev-pt (copy-point pt :temporary)
                      pt (form-offset pt 1))
                (when (or (null pt) (point>= pt end))
                  (return))
                ;; When there's multi-line form, re-indent all lines in it
                (when (> (count #\Newline (points-to-string prev-pt pt)) 1)
                  (indent-region-for-commands prev-pt pt nil))))))))

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
                           (delete-horizontal-space sub-pt)
                           (if (eql (character-at sub-pt 0) #\))
                               (return-from current-form)
                             (let* ((prev   (subseq columns line-start i))
                                    (target (+ item-start-column (apply #'+ (or prev '(0))) (length prev)))
                                    (offset (- target (point-column sub-pt))))
                               (cond ((plusp offset)  (insert-spaces sub-pt offset))
                                     ((minusp offset) (delete-characters sub-pt offset)))))
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
              (cond ((eql (character-at point 0) #\() (point-after point))
                    ((null (form-offset (copy-point point :temporary) -1 t 1)) t)
                    (t (loop for r = (form-offset point -1) until (null r))))
              (let ((sym (if (eql (character-at point 0) #\() nil
                           (get-symbol-from-point point :create-new nil))))
                (point-before point)
                (cond ((eq sym 'defclass)       (align-defclass      point))
                      ((member sym '(let let*)) (align-let           point))
                      ((eq sym 'make-instance)  (align-make-instance point))
                      ((fboundp sym)            (align-list          point t))
                      (t                        (align-list          point))))))
        (error (e) (editor-error "Error while aligning: ~A" e))))))

