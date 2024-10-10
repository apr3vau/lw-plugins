;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

(defsystem lw-plugins
  :author "April & May"
  :license "0BSD"
  :components ((:file "colourful")
               (:file "flex-complete")
               (:file "directory")
               (:file "expand-region")
               (:file "editor-markdown")
               (:file "pair")))

(defsystem lw-plugins/nerd-icons
  :author "April & May"
  :license "0BSD"
  :components ((:file "nerd-icons/nerd-icons")))

(defsystem lw-plugins/nerd-icons-directory
  :author "April & May"
  :license "0BSD"
  :depends-on (lw-plugins/nerd-icons)
  :components ((:file "nerd-icons/nerd-icons-directory")))
