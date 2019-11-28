(load "vector.lisp")
(load "particle.lisp")

(defpackage :ep10
  (:use :cl :sketch :vector :particle))
(in-package :ep10)


(defparameter *width* 800)
(defparameter *height* 600)
(defparameter *ship* (make-instance 'particle :x (/ *width* 2) :y (/ *height* 2) :speed 0 :direction 0))
(defparameter *thrust* (make-instance 'vector2 :x 0 :y 0))
(defparameter *angle* 0)
(defparameter *turning* 0)  ; -1 = left, 1 = right
(defparameter *thrusting* nil)


(defun process-ship ()
  (case *turning*
    ((-1) (decf *angle* 0.05))
    ((1) (incf *angle* 0.05)))
  (set-angle *thrust* *angle*)
  ; (if *thrusting*
  ;   (set-length *thrust* 0.001)
  ;   (set-length *thrust* 0))
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
    (let ((x (get-x position)) (y (get-y position)))
      (with-current-matrix
        (translate x y)
        (rotate (degrees *angle*))
        (with-pen (make-pen :stroke +orange+)
          (polyline 16 0 2 -4 2 -7 5 -10 -4 -12 -6 -10 -6 10 -4 12 5 10 2 7 2 4 16 0))
        (if *thrusting*
          (with-pen (make-pen :stroke +red+)
            (polyline -6 -6 -13 -4 -10 -2 -16 0 -10 2 -13 4 -6 6)))))))


(defsketch ep10 ((title "Episode 10") (width *width*) (height *height*))
  (process-ship)
  (draw-ship))

(defmethod kit.sdl2:keyboard-event ((win ep10) state ts repeatp keysym)
  (unless repeatp
    (let ((scancode (sdl2:scancode keysym)))
      (when (eq state :keydown)
        (case scancode
          ((:scancode-w :scancode-up) (setf *thrusting* t))
          ((:scancode-a :scancode-left) (decf *turning*))
          ((:scancode-d :scancode-right) (incf *turning*))))
      (when (eq state :keyup)
        (case scancode
          ((:scancode-w :scancode-up) (setf *thrusting* nil))
          ((:scancode-a :scancode-left) (incf *turning*))
          ((:scancode-d :scancode-right) (decf *turning*)))))))


(defun show() (make-instance 'ep10))

(defun run ()
  #-sbcl (show)
  #+sbcl (sdl2:make-this-thread-main #'show))
