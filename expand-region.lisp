;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

;; I love Emacs's expand-region plugin, so I implement my own in
;; LispWorks. It's currently only available for Lisp, unable to
;; decrease selections, and will mark the whole symbol for minimum
;; range.

;; Usage: Load this file, The "Expand Region" command will
;; automatically bind to C-=.

(in-package editor)

;; 02Oct24: Refactored. Support expands to word, prefix and upper string
(defun expand-region (start end)
  "Core function of expand region.

Giving two points (START END), moves the points to expanded position
and returns them.

The expansion criteria is determined by the position and inner string
of the points, can be expanded to current word, current form, upper
form, upper string, or the whole buffer."
  (let ((str (points-to-string start end))
        (prev-char (character-at start -1))
        (next-char (character-at end 0)))
    (cond ((or (and (point= start end)
                    (or (and prev-char (alphanumericp prev-char))
                        (and next-char (alphanumericp next-char))))
               (and (every #'alphanumericp str)
                    (not (and (plusp (character-attribute :word-delimiter prev-char))
                              (plusp (character-attribute :word-delimiter next-char))))))
           ;; Expand to word
           (if (zerop (character-attribute :word-delimiter next-char))
               (progn
                 (word-offset start 1)
                 (word-offset start -1)
                 (word-offset end 1))
             (progn
               (word-offset start -1)
               (move-point end start)
               (word-offset end 1))))
          ((or (point= start end)
               (and (every (lambda (c) (eq (character-attribute :lisp-syntax c) :constituent)) str)
                    (or (eq (character-attribute :lisp-syntax prev-char) :constituent)
                        (eq (character-attribute :lisp-syntax next-char) :constituent))))
           ;; Expand to form
           
           ;; 04Oct24: Due to a bug of FORM-OFFSET, traverse throught
           ;; a list which has sub-list with #\' or #\` prefixing
           ;; using :NOT-PREFIX T will cause the point move wrongly.
           ;; The feature of expanding to prefix will be commented
           ;; until the bug being fixed.
           (if (whitespace-char-p prev-char)
               (progn
                 (form-offset start 1)
                 (form-offset start -1)
                 (move-point end start)
                 (form-offset end 1))
             (progn
               (form-offset start -1)
               (move-point end start)
               (form-offset end 1))))
          #|((eq (character-attribute :lisp-syntax prev-char) :prefix)
           ;; Expand to prefix
           (move-point start end)
           (form-offset start -1)
           (move-point end start)
           (form-offset end 1))|#
          (t (if-let (up (form-offset start -1 t 1 t))
                 ;; Expand to Upper form
                 (progn
                   (move-point end start)
                   (form-offset end 1))
               (if-let (string-quote (loop for i downfrom -1
                                           for char = (character-at start i)
                                           until (null char)
                                           when (eq (character-attribute :lisp-syntax char) :string-quote)
                                             do (return i)))
                   ;; Expand outside from string
                   (progn
                     (character-offset start string-quote)
                     (move-point end start)
                     (form-offset end 1))
                 ;; Expand to the whole buffer
                 (progn
                   (buffer-start start)
                   (buffer-end end))))))
    (values start end)))

(defcommand "Expand Region" (p)
     "Expand the marking region.

Sequentially expand to current word, current form, upper form, until
the whole buffer, by involking the command repeatly or with a prefix
argument."
     "Expand the marking region.

Sequentially expand to current word, current form, upper form, until
the whole buffer, by involking the command repeatly or with a prefix
argument."
  (loop repeat (or p 1)
        do (let ((region-p (buffer-region-highlighted-p (current-buffer))))
             (with-point ((end (if region-p (current-mark) (current-point))))
               (expand-region (current-point) end)
               (set-current-mark end)
               (set-highlight-buffer-region t)))))

(bind-key "Expand Region" "Control-=" :global)