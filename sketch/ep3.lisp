(defpackage :ep3
  (:use :cl :sketch))
(in-package :ep3)

(defparameter *speed* 0.1)
(defparameter *radius* 20)

(defsketch ep3-oscillate ((title "Episode 3 - Oscillate") (width 400) (height 400) (angle 0))
  (incf angle *speed*)
  (let ((center-y (/ height 2)) (offset (* height 0.4)))
    (circle (/ width 2) (+ center-y (* (sin angle) offset)) *radius*)))
        
(defsketch ep3-grow ((title "Episode 3 - Grow") (width 400) (height 400) (angle 0))
  (incf angle *speed*)
  (circle (/ width 2) (/ height 2) (+ *radius* (* 10 (sin angle)))))

(defsketch ep3-fade ((title "Episode 3 - Fade") (width 400) (height 400) (angle 0))
  (incf angle *speed*)
  (with-pen (make-pen :fill (rgb 1 1 1 (+ 0.5 (* 0.5 (sin angle)))))
    (circle (/ width 2) (/ height 2) *radius*)))


(make-instance 'ep3-oscillate)
(make-instance 'ep3-grow)
(make-instance 'ep3-fade)
