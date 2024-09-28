;;; Copyright (c) 2024, April Simone
;;; SPDX-License-Identifier: BSD-2-Clause

(in-package editor)

(defcommand "Expand Region" (p)
     ""
     ""
  (loop repeat (or p 1)
        do (let ((region-p (buffer-region-highlighted-p (current-buffer))))
             (with-point ((start (current-point))
                                 (end (if region-p (current-mark) (current-point))))
               (let* ((former-character-type (character-attribute :lisp-syntax (character-at start -1)))
                      (latter-character-type (character-attribute :lisp-syntax (character-at end 0)))
                      (region-constituent-p (and region-p
                                                 (every (lambda (c)
                                                          (eq (character-attribute :lisp-syntax c) :constituent))
                                                        (points-to-string start end))))
                      (symbol-not-covered (and region-constituent-p
                                               (or (eq former-character-type :constituent)
                                                   (eq latter-character-type :constituent)))))
                 (cond
                  ;; Start point move to previous form's start
                  ((and (not region-p)
                        (not (member former-character-type '(:space :newline)))
                        (member latter-character-type '(:space :newline)))
                   (form-offset start -1))
                  ;; Start point move to current form's start
                  ((or (not region-p) symbol-not-covered)
                   (form-offset start 1)
                   (form-offset start -1))
                  ;; Start point move to upper form's start
                  (t
                   (form-offset start -1 t 1 t)))
                 (move-point end start)
                 (form-offset end 1)
                 (move-point (current-point) start)
                 (set-current-mark end)
                 (set-highlight-buffer-region t))))))

(bind-key "Expand Region" "Control-=" :global)