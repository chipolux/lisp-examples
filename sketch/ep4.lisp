(defpackage :ep4
  (:use :cl :sketch))
(in-package :ep4)

(defparameter *width* 400)
(defparameter *height* *width*)
(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))

(defparameter *speed* 0.01)
(defparameter *radius* 100)

(defparameter *x-radius* 100)
(defparameter *y-radius* 200)

(defsketch ep4-circular ((title "Episode 4 - Circular") (width *width*) (height *height*) (angle 0))
  (incf angle *speed*)
  (let ((x (+ (* (cos angle) *radius*) *center-x*))
        (y (+ (* (sin angle) *radius*) *center-y*)))
    (circle x y 10)))

(defsketch ep4-elliptical ((title "Episode 4 - Elliptical") (width *width*) (height *height*) (angle 0))
  (incf angle *speed*)
  (let ((x (+ (* (cos angle) *x-radius*) *center-x*))
        (y (+ (* (sin angle) *y-radius*) *center-y*)))
    (circle x y 10)))

(defsketch ep4-lissajous ((title "Episode 4 - Lissajous")
                           (width *width*)
                           (height *height*)
                           (x-angle 0)
                           (y-angle 0)
                           (x-speed .05)
                           (y-speed .06))
  (incf x-angle x-speed)
  (incf y-angle y-speed)
  (let ((x (+ (* (cos x-angle) *radius*) *center-x*))
        (y (+ (* (sin y-angle) *radius*) *center-y*)))
    (circle x y 10)))
        
(make-instance 'ep4-circular)
(make-instance 'ep4-elliptical)
(make-instance 'ep4-lissajous)
