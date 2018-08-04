(load "vector.lisp")
(load "particle.lisp")

(defpackage :ep10
  (:use :cl :sketch :vector :particle))
(in-package :ep10)


(defparameter *width* 800)
(defparameter *height* 800)
(defparameter *ship* (make-instance 'particle :x (/ *width* 2) :y (/ *height* 2) :speed 0 :direction 0))
(defparameter *thrust* (make-instance 'vector2 :x 0 :y 0))
(defparameter *angle* 0)


(defun process-ship ()
  (accelerate *ship* *thrust*)
  (update *ship*)
  (with-slots (position) *ship*
    (if (> (get-x position) *width*)
      (set-x position 0))
    (if (< (get-x position) 0)
      (set-x position *width*))
    (if (> (get-y position) *height*)
      (set-y position 0))
    (if (< (get-y position) 0)
      (set-y position *height*))))

(defun draw-ship ()
  (with-slots (position) *ship*
    (circle (get-x position) (get-y position) 10)))


(defsketch ep10 ((title "Episode 10") (width *width*) (height *height*))
  (process-ship)
  (draw-ship))

(defmethod kit.sdl2:keyboard-event ((win ep10) state ts repeatp keysym)
  (unless repeatp
    (let ((scancode (sdl2:scancode keysym)))
      (when (eq state :keydown)
        (case scancode
          ((:scancode-w :scancode-up) (set-y *thrust* -0.1))
          ((:scancode-s :scancode-down) (set-y *thrust* 0.1))
          ((:scancode-a :scancode-left) (set-x *thrust* -0.1))
          ((:scancode-d :scancode-right) (set-x *thrust* 0.1))))
      (when (eq state :keyup)
        (case scancode
          ((:scancode-w :scancode-up) (set-y *thrust* 0))
          ((:scancode-s :scancode-down) (set-y *thrust* 0))
          ((:scancode-a :scancode-left) (set-x *thrust* 0))
          ((:scancode-d :scancode-right) (set-x *thrust* 0)))))))


(make-instance 'ep10)
