(load "vector.lisp")
(load "particle.lisp")

(defpackage :ep9
  (:use :cl :sketch :vector :particle))
(in-package :ep9)


(defparameter *width* 1280)
(defparameter *height* 900)
(defparameter *particle* (make-instance 'particle :x 0 :y *height* :speed 10 :direction (- (radians 90))))
(defparameter *accel* (make-instance 'vector2 :x 0.1 :y 0.1))
(defparameter *gravity* (make-instance 'vector2 :x 0 :y 0.1))

(defun make-particle ()
  (let ((x (/ *width* 2))
        (y (/ *height* 3))
        (s (+ (* (random 1.0) 5) 2))
        (d (* (random 1.0) pi 2)))
    (make-instance 'particle :x x :y y :speed s :direction d)))

(defun make-particle-with-gravity ()
  (let ((x (/ *width* 2))
        (y (/ *height* 3))
        (s (+ (* (random 1.0) 5) 2))
        (d (* (random 1.0) pi 2)))
    (make-instance 'particle :x x :y y :speed s :direction d :gravity 0.1)))

(defun process-particle (particle &optional accel)
  (if accel (accelerate particle accel))
  (update particle)
  (with-slots (position) particle
    (circle (get-x position) (get-y position) 5)))

(defsketch ep9-ball ((title "Episode 9 - Ball") (width *width*) (height *height*))
  (process-particle *particle* *accel*))

(defsketch ep9-fireworks ((title "Episode 9 - Fireworks")
                          (width *width*) (height *height*)
                          (particles (loop repeat 100 collect (make-particle))))
  (loop for particle in particles do (process-particle particle *gravity*)))

(defsketch ep9-fireworks2 ((title "Episode 9 - Fireworks 2")
                          (width *width*) (height *height*)
                          (particles (loop repeat 100 collect (make-particle-with-gravity))))
  (loop for particle in particles do (process-particle particle)))


(make-instance 'ep9-ball)
(make-instance 'ep9-fireworks)
(make-instance 'ep9-fireworks2)
