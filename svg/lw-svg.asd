(defsystem lw-svg
  :author "April & May"
  :license "0BSD"
  :depends-on (alexandria cl-ppcre plump serapeum)
  :components ((:file "lw-svg")))

(defsystem lw-svg-test
  :author "April & May"
  :license "0BSD"
  :depends-on (lw-svg parachute)
  :components ((:file "lw-svg-test")))
