;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

;; Editor stream that partly support ANSI escape sequences.
;; Currently unstable, not fully tested. 

;; Usage: Making an instance of ESCAPED-EDITOR-STREAM with an
;; associate editor buffer :BUFFER, then redirect program output to
;; this stream. The stream will write ANSI escaped content into
;; BUFFER.

(defpackage escaped-editor-stream
  (:add-use-defaults))
(in-package :escaped-editor-stream)

(defclass escaped-editor-stream
          (stream:fundamental-character-output-stream)
  ((buffer :initarg :buffer)
   (face :initform (editor:make-face nil))
   (pending-sequence :initform nil)))
(export 'escaped-editor-stream)

(defmethod stream-element-type ((stream escaped-editor-stream))
  'character)

(defmethod stream:stream-write-char ((stream escaped-editor-stream)
                                     char)
  (with-slots (buffer face pending-sequence) stream
    (let ((pt (editor:buffer-point buffer)))
      (cond
       ((eql char #\Escape)
        (if (equal pending-sequence '(#\Escape))
            (progn (setf pending-sequence nil)
              (editor:insert-character pt #\Escape))
          (push-end #\Escape pending-sequence)))
       (pending-sequence
        (push-end char pending-sequence)
        (prog ((pending (cdr pending-sequence))
               (csi-param 0)
               (csi-param-2 0))
          (case (pop pending)
            (nil (return))
            (#\[ (go csi-get-param))
            (t (go finish)))
          csi-get-param
          (let ((c (car pending)))
            (cond ((null c) (return))
                  ((digit-char-p c)
                   (setq csi-param (+ (* 10 csi-param)
                                      (parse-integer (string (pop pending)))))
                   (go csi-get-param))
                  (t (go csi-term))))
          csi-get-param-2
          (let ((c (car pending)))
            (cond ((null c) (return))
                  ((digit-char-p c)
                   (setq csi-param-2 (+ (* 10 csi-param-2)
                                      (parse-integer (string (pop pending)))))
                   (go csi-get-param-2))
                  (t (go csi-term))))
          csi-term (case (pop pending)
                     (nil (return))
                     (#\; (go csi-get-param-2))
                     (#\A (go cuu))
                     (#\B (go cud))
                     (#\C (go cuf))
                     (#\D (go cub))
                     (#\E (go cnl))
                     (#\F (go cpl))
                     (#\G (go cha))
                     (#\H (go cup))
                     (#\J (go ed))
                     (#\K (go el))
                     (#\m (go sgr))
                     (t (go finish)))
          cuu (editor:line-offset pt (- 0 csi-param) (editor:point-column pt))
          (go finish)
          cud (editor:line-offset pt csi-param (editor:point-column pt))
          (go finish)
          cuf (editor:character-offset pt csi-param)
          (go finish)
          cub (editor:character-offset pt (- 0 csi-param))
          (go finish)
          cnl (editor:line-offset pt csi-param 0)
          (go finish)
          cpl (editor:line-offset pt (- 0 csi-param) 0)
          (go finish)
          cha (editor:character-offset pt (- csi-param (editor:point-column pt)))
          (go finish)
          cup
          (editor:goto-line buffer csi-param)
          (editor:character-offset pt csi-param-2)
          (go finish)
          ed (case csi-param
               (0 (editor:with-point ((end pt))
                    (editor:buffer-end end)
                    (editor:delete-between-points pt end)))
               (1 (editor:with-point ((start pt))
                    (editor:buffer-start start)
                    (let ((row (editor:point-line-offset pt))
                          (col (editor:point-column pt)))
                      (editor:delete-between-points start pt)
                      (editor:insert-string
                       pt
                       (string-append (make-string row :initial-element #\Newline)
                                      (make-string col :initial-element #\Space))))))
               (2 (editor:with-point ((start pt)
                                      (end pt))
                    (editor:buffer-start start)
                    (editor:buffer-end end)
                    (let ((row (editor:point-line-offset pt))
                          (col (editor:point-column pt)))
                      (editor:delete-between-points start end)
                      (editor:insert-string
                       pt
                       (string-append (make-string row :initial-element #\Newline)
                                      (make-string col :initial-element #\Space))))
                    (editor:delete-between-points start pt))))
          (go finish)
          el (case csi-param
               (0 (editor:with-point ((end pt))
                    (editor:line-end end)
                    (editor:delete-between-points pt end)))
               (1 (editor:with-point ((start pt))
                    (editor:line-start start)
                    (let ((col (editor:point-column pt)))
                      (editor:delete-between-points start pt)
                      (editor:insert-string pt (make-string col :initial-element #\Space)))))
               (2 (editor:with-point ((start pt)
                                      (end pt))
                    (editor:line-start start)
                    (editor:line-end end)
                    (let ((col (editor:point-column pt)))
                      (editor:delete-between-points start end)
                      (editor:insert-string pt (make-string col :initial-element #\Space))))))
          (go finish)
          sgr (macrolet
                  ((setface (key val)
                     `(let ((props (list :bold-p (editor::face-bold-p face)
                                         :italic-p (editor::face-italic-p face)
                                         :underline-p (editor::face-underline-p face)
                                         :inverse-p (editor::face-inverse-p face)
                                         :foreground (editor::face-foreground face)
                                         :background (editor::face-background face)
                                         :font (editor::face-font face))))
                        (setf (getf props ,key) ,val)
                        (setf face
                              (apply #'editor:make-face nil props)))))
                (case csi-param
                  (0 (setf face nil))
                  (1 (setface :bold-p t))
                  (3 (setface :italic-p t))
                  (4 (setface :underline-p t))
                  (30 (setface :foreground :black))
                  (31 (setface :foreground :red3))
                  (32 (setface :foreground :green3))
                  (33 (setface :foreground :yellow3))
                  (34 (setface :foreground :blue3))
                  (35 (setface :foreground :magenta3))
                  (36 (setface :foreground :cyan3))
                  (37 (setface :foreground :gray80))
                  (90 (setface :foreground :gray))
                  (91 (setface :foreground :red))
                  (92 (setface :foreground :green))
                  (93 (setface :foreground :yellow))
                  (94 (setface :foreground :blue))
                  (95 (setface :foreground :magenta))
                  (96 (setface :foreground :cyan))
                  (97 (setface :foreground :white))

                  (40 (setface :background :black))
                  (41 (setface :background :red3))
                  (42 (setface :background :green3))
                  (43 (setface :background :yellow3))
                  (44 (setface :background :blue3))
                  (45 (setface :background :magenta3))
                  (46 (setface :background :cyan3))
                  (47 (setface :background :gray80))
                  (100 (setface :background :gray))
                  (101 (setface :background :red))
                  (102 (setface :background :green))
                  (103 (setface :background :yellow))
                  (104 (setface :background :blue))
                  (105 (setface :background :magenta))
                  (106 (setface :background :cyan))
                  (107 (setface :background :white))

                  (39 (setface :foreground nil))
                  (49 (setface :background nil)))
                (go finish))
          finish
          (setf pending-sequence nil)
          (return)))
       (t (unless (member (editor:character-at pt 0) '(#\Return #\Newline))
            (editor:with-point ((end pt))
              (editor:character-offset end 1)
              (editor:delete-between-points pt end)))
          (editor:with-point ((old (editor:copy-point pt :temporary)))
            (editor::insert-character pt char)
            (editor:put-text-property old pt 'editor:face face)))))))