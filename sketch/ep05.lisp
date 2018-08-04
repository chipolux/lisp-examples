(defpackage :ep5
  (:use :cl :sketch))
(in-package :ep5)

(defparameter *width* 400)
(defparameter *height* *width*)
(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))

(defparameter *static-angle* 0)
(defparameter *mobile-angle* 0)
(defparameter *mobile-x* 0)
(defparameter *mobile-y* 0)

(defun draw-arrow (x y angle)
  (with-current-matrix
    (with-pen (make-pen :stroke +orange+)
      (translate x y)
      (rotate angle)
      (line 10 0 -10 0)
      (line 10 0 5 5)
      (line 10 0 5 -5))))

(defsketch ep5-static-arrow ((title "Episode 5.1 - Static Arrow") (width *width*) (height *height*))
  (draw-arrow *center-x* *center-y* (degrees *static-angle*)))

(defmethod kit.sdl2:mousemotion-event ((win ep5-static-arrow) timestamp mask x y xrel yrel)
  (setf *static-angle* (atan (- y *center-y*) (- x *center-x*))))

(defsketch ep5-mobile-arrow ((title "Episode 5.2 - Mobile Arrow")
                               (width *width*)
                               (height *height*)
                               (angle 0))
  (incf angle 0.03)
  (setf *mobile-x* (+ (* (cos angle) 100) *center-x*))
  (setf *mobile-y* (+ (* (sin angle) 100) *center-y*))
  (draw-arrow *mobile-x* *mobile-y* (degrees *mobile-angle*)))

(defmethod kit.sdl2:mousemotion-event ((win ep5-mobile-arrow) timestamp mask x y xrel yrel)
  (setf *mobile-angle* (atan (- y *mobile-y*) (- x *mobile-x*))))


(make-instance 'ep5-static-arrow)
(make-instance 'ep5-mobile-arrow)
