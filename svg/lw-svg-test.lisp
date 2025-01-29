(defpackage lw-svg-test
  (:use :cl :lw-svg :parachute))

(in-package lw-svg-test)

(define-test+run test-css-parse-numeric-color
  (is color:colors=
      (lw-svg::css-parse-numeric-color "rgb(255, 0, 0)")
      (color:make-rgb 1 0 0))
  (is color:colors=
      (lw-svg::css-parse-numeric-color "rgb(100%, 0%, 0%)")
      (color:make-rgb 1 0 0))
  (is color:colors=
      (lw-svg::css-parse-numeric-color "rgba(0,0,255,0.5)")
      (color:make-rgb 0 0 1 0.5)))

(define-test+run test-css-parse-color
  (is color:colors=
      (lw-svg::css-parse-color "transparent")
      (color:make-rgb 1 1 1 0))
  (is color:colors=
      (lw-svg::css-parse-color "rgba(0,0,255,0.5)")
      (color:make-rgb 0 0 1 0.5))
  (is color:colors=
      (lw-svg::css-parse-color "#ff0000")
      (color:make-rgb 1 0 0))
  (is color:colors=
      (lw-svg::css-parse-color "#f00")
      (color:make-rgb 1 0 0))
  (false (lw-svg::css-parse-color "none")))

(define-test+run test-css-parse-a-number
  (is-values (lw-svg::css-parse-a-number "-12.5,13")
    (= -12.5)
    (= 5))
  (is-values (lw-svg::css-parse-a-number "2.5e12 13")
      (= 2.5d12)
      (= 6))
  (is =
      (lw-svg::css-parse-a-number "-12-13")
      -12)
  (is eql
      (lw-svg::css-parse-a-number "-e")
      nil))

(define-test+run test-css-parse-all-numbers-from-string
  (is =
      (length (lw-svg::css-parse-all-numbers-from-string "850,75  958,137.5 958,262.5
                    850,325 742,262.6 742,137.5"))
      12))

(define-test+run test-css-parse-angel
  (is-values (lw-svg::css-parse-angel "-45degtest")
    (= (- (/ gp:pi-by-2 2)))
    (= 6))
  (is-values (lw-svg::css-parse-angel "4e4gradtest")
    (= (lw-svg::deg-to-rad 3.6D4))
    (= 7)))

(define-test+run test-parse-svg-path-data
  (let ((commands (lw-svg::svg-parse-path-data "M200,300 L400,50 L600,300
           L800,550 L1000,300")))
    (is = (length commands) 5)
    (loop for c across commands
          do (every (lambda (n) (of-type double-float n))
                    (lw-svg::svg-path-command-args c)))))

;; Interactive test

(defparameter *interactive-tests*
  '(("Path quadratic bezier" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/paths/quad01.svg")
    ("Path arc" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/paths/arcs01.svg")
    ("Retangle and transform" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/shapes/rect02.svg")
    ("Ellipse and transform" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/shapes/ellipse01.svg")
    ("Line and stroke-width" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/shapes/line01.svg")
    ("Polyline" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/shapes/polyline01.svg")
    ("Polygon" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/shapes/polygon01.svg")
    ("Dashed stroke" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/painting/dashes.svg")
    ("Fill rule nonzero" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/painting/fillrule-nonzero.svg")
    ("Fill rule evenodd" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/painting/fillrule-evenodd.svg")
    ("Groups and Opacity" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/masking/opacity01.svg")
    ("Linear gradient" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/pservers/lingrad01.svg")
    ("Radial gradient" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/pservers/radgrad01.svg")))

(capi:define-interface interactive-test-interface ()
  ((i :initform 0)
   (name :initform (first (first *interactive-tests*)))
   (url :initform (second (first *interactive-tests*)))
   (wrong :initform nil))
  (:panes
   (browser
    capi:browser-pane
    :url url
    :visible-min-width 500
    :visible-min-height 500)
   (output
    capi:output-pane
    :visible-min-width 500
    :visible-min-height 500
    :background :white
    :foreground :black
    :display-callback (lambda (pane x y w h)
                        (declare (ignore x y w h))
                        (draw-svg-from-string
                         pane
                         (dex:get (slot-value (capi:element-interface pane) 'url) :force-string t)
                         0 0)))
   (buttons
    capi:push-button-panel
    :items '(:yes :no)
    :selection-callback
    (lambda (data itf)
      (with-slots (i name url browser output wrong) itf
        (when (eql data :no)
          (push name wrong))
        (incf i)
        (if (>= i (length *interactive-tests*))
          (progn
            (if wrong
              (capi:prompt-with-list
                 wrong
                 (format nil "Test finished with ~A/~A errors." (length wrong) (length *interactive-tests*)))
              (capi:prompt-with-message "All test are passed!"))
            (capi:quit-interface itf))
          (progn
            (setf (capi:interface-title itf) (format nil "~A/~A ~A"
                                                     (1+ i) (1+ (length *interactive-tests*))
                                                     (first (nth i *interactive-tests*)))
                  url (second (nth i *interactive-tests*)))
            (capi:browser-pane-navigate browser url)
            (gp:invalidate-rectangle output)))))))
  (:layouts
   (main-layout
    capi:column-layout
    '(displayers-row buttons)
    :title "Compare if two images are same"
    :title-position :frame
    :adjust :center)
   (displayers-row
    capi:row-layout
    '(browser output)))
  (:default-initargs
   :title (format nil "1/~A ~A" (1+ (length *interactive-tests*)) (first (first *interactive-tests*)))))

(capi:display (make-instance 'interactive-test-interface))

#|
(capi:contain
 (make-instance
  'capi:output-pane
  :display-callback
  (lambda (port x y w h)
    (loop for i from 0
          for svg in (serapeum:repeat-sequence (directory "~/svg-test/*.svg") 4)
          do (multiple-value-bind (y x) (floor i 24)
               (draw-svg-from-string port svg (* x 32) (* y 32)))))))

(setq test (capi:prompt-for-string ""))

(capi:contain
 (make-instance
  'capi:output-pane
  :display-callback
  (lambda (port x y w h)
    (draw-svg-from-string port test 0 0))))
  |#