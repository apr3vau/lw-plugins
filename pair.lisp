;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

;; We make a simplify version of electric-pair-mode here. For paired
;; characters in *PAIRS*, inserting the left one will automatically
;; append the right one, deleting the left one will also automatically
;; delete the right one if the right one is straightly following.

;; Support Unicode characters.

;; Usage: Load this file. Reload is needed if you modified *PAIRS* afterwards.

(in-package editor)

(defvar *pairs*
  '((#\( #\))
    (#\" #\")
    (#\[ #\])
    (#\{ #\})
    (#\| #\|)
    ))

;; Insertion
(loop for (1st 2nd) in *pairs*
      for command-name = (format nil "Insert ~A ~A For Selection" (char-code 1st) (char-code 2nd))
      do (eval `(defcommand ,command-name (p)
                     ,command-name ,command-name
                  (insert-balanced-text-for-selection p ,(string 1st) ,(string 2nd))))
         (bind-key command-name 1st :global))

;; Deletion
(defcommand "Delete Previous Character With Paren" (p)
     "Delete the right one of the pair if the right one is straightly following."
     "Delete the right one of the pair if the right one is straightly following."
  (loop with point = (current-point)
        repeat (or p 1)
        for pair = (assoc (character-at point -1) *pairs*)
        do (if (and pair
                    (eql (character-at point 0) (second pair)))
               (progn
                 (delete-previous-character-command 1)
                 (delete-next-character-command 1))
               (delete-previous-character-command 1))))

(bind-key "Delete Previous Character With Paren" #\Backspace :global)
