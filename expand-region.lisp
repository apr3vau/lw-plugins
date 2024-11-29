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
;; 11Oct24: Refactored using PROG. Fix the support of expanding to string
;; 29Nov24: Simply use face to judge strings...
(defun expand-region (start end)
  "Core function of expand region.

Giving two points (START END), moves the points to expanded position
and returns them.

The expansion criteria is determined by the position and inner string
of the points, can be expanded to current word, current form, upper
form, upper string, or the whole buffer."
  (prog ((str (points-to-string start end))
         (prev-char (character-at start -1))
         (next-char (character-at end 0)))
    (cond ((or (and (point= start end)
                    (or (and prev-char (alphanumericp prev-char))
                        (and next-char (alphanumericp next-char))))
               (and (every #'alphanumericp str)
                    (not (and (plusp (character-attribute :word-delimiter prev-char))
                              (plusp (character-attribute :word-delimiter next-char))))))
           (if (zerop (character-attribute :word-delimiter next-char))
               (go word-current)
             (go word-before)))
          ((and (eql prev-char #\")
                (not (eql (character-at start -2) #\\))
                (eql next-char #\")
                (not (eql (character-at end 1) #\\)))
           (go upper-string))
          ((or (point= start end)
               (and (every (lambda (c) (eq (character-attribute :lisp-syntax c) :constituent)) str)
                    (or (eq (character-attribute :lisp-syntax prev-char) :constituent)
                        (eq (character-attribute :lisp-syntax next-char) :constituent))))
           (if (or (whitespace-char-p prev-char)
                   (eq (character-attribute :lisp-syntax prev-char) :prefix))
               (go form-current)
             (go form-before)))
          ((equalp (get-text-property start 'face) *font-lock-string-face*)
           (go string-content))
          ((or (form-offset start -1)
               (form-offset end 1))
           (go list-content))
          (t (go upper-form)))
    word-before
    (word-offset start -1)
    (move-point end start)
    (word-offset end 1)
    (go end)
    word-current
    (word-offset end 1)
    (move-point start end)
    (word-offset start -1)
    (go end)
    form-before
    (form-offset start -1)
    (move-point end start)
    (form-offset end 1)
    (go end)
    form-current
    (form-offset start 1)
    (form-offset start -1)
    (move-point end start)
    (form-offset end 1)
    (go end)
    string-content
    (loop until (and (eql (character-at start -1) #\")
                     (not (eql (character-at start -2) #\\)))
          do (point-before start))
    (loop until (and (eql (character-at end 0) #\")
                     (not (eql (character-at end 1) #\\)))
          do (point-after end))
    (go end)
    upper-string
    (point-before start)
    (point-after end)
    (go end)
    list-content
    (loop while (form-offset start -1))
    (loop while (form-offset end 1))
    (go end)
    upper-form
    (form-offset start -1 t 1 t)
    (move-point end start)
    (form-offset end 1)
    (go end)
    end
    (values start end))
)

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
