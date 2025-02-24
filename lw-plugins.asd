;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

(defsystem lw-plugins
  :author "April & May"
  :license "0BSD"
  :components ((:file "align")
               (:file "colourful")
               (:file "directory")
               (:file "docstring-folding")
               (:file "editor-markdown")
               (:file "expand-region")
               (:file "flex-complete")
               (:file "highlight")
               (:file "line-number")
               (:file "pair")
               (:file "side-tree")
               (:file "vertical-prompt")
               (:file "visual-line")
               (:file "yank-from-kill-ring")))

(defsystem lw-plugins/nerd-icons
  :author "April & May"
  :license "0BSD"
  :components ((:file "nerd-icons/nerd-icons")))

(defsystem lw-plugins/nerd-icons-directory
  :author "April & May"
  :license "0BSD"
  :depends-on (lw-plugins/nerd-icons)
  :components ((:file "nerd-icons/nerd-icons-directory")))

(defsystem lw-plugins/nerd-icons-side-tree
  :author "April & May"
  :license "0BSD"
  :depends-on (lw-plugins/nerd-icons)
  :components ((:file "nerd-icons/nerd-icons-side-tree")))

(defsystem lw-plugins/nerd-icons-vertical-prompt
  :author "April & May"
  :license "0BSD"
  :depends-on (lw-plugins/nerd-icons)
  :components ((:file "nerd-icons/nerd-icons-vertical-prompt")))
