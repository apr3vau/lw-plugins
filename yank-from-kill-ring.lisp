;; Copyright (c) 2025, April & May
;; SPDX-License-Identifier: 0BSD

;; This small plugin replicate the "yank-from-kill-ring" behavior
;; after Emacs 28 (Pop-up paste selection with M-y).

;; Notes that we do not support rotating kill-ring after "Yank From
;; Kill Ring" command, as the kill-ring and copy-paste function behave
;; differently between LispWorks and Emacs, which may result unwanted
;; behaviours after replication.

(in-package editor)

(defcommand "Yank From Kill Ring" (p)
     "Select a stretch of previously killed text and insert (\"paste\") it."
     "This function will not behave in any reasonable fashion when
  called as a lisp function."
  (declare (ignore p))
  (editor:complete-in-place
   (lambda (&rest ignore)
     (declare (ignore ignore))
     (or (loop for i from 0 below (ring-length *kill-ring*)
               collect (ring-ref *kill-ring* i))
         (editor-error "Kill ring is empty")))
   :print-function (lambda (str)
                     (when (buffer-string-p str)
                       (setq str (buffer-string-string str)))
                     (loop with new-str = (make-array 15 :element-type 'character :fill-pointer 0 :adjustable t)
                           for c across str
                           for prev-whitespace-p = t then whitespace-p
                           for whitespace-p = (whitespace-char-p c)
                           do (cond ((member c '(#\Return #\Newline))
                                     (vector-push-extend #\⏎ new-str))
                                    ((and prev-whitespace-p whitespace-p) nil)
                                    (t (vector-push-extend c new-str)))
                           finally (return new-str)))
   :extract-func (constantly "")
   :insert-func (lambda (result &rest ignore)
                  (declare (ignore ignore))
                  (if (buffer-string-p result)
                    (buffer-string-string result)
                    (or result (editor-error "Abort"))))))

(defadvice (rotate-kill-ring-command lw-plugins :around) (p)
  (if (not (eq (last-command-type) :unkill))
    (yank-from-kill-ring-command p)
    (call-next-advice p)))
