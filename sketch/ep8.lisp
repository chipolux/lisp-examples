(load "vector.lisp")
(load "particle.lisp")

(defpackage :ep8
  (:use :cl :sketch :vector :particle))
(in-package :ep8)

;; note: in the video they use pi / 6, which is ~0.5 radians, or 30 degrees.
;;       sketch provides the radians and degrees conversion functions so I just
;;       use those here instead of the pi math.
(defparameter *width* 400)
(defparameter *height* *width*)
(defparameter *position* (make-instance 'vector2 :x 100 :y 100))
(defparameter *velocity* (make-instance 'vector2 :length 3 :angle (radians 30)))
(defparameter *particle* (make-instance 'particle :x 100 :y 100 :speed 3 :direction (radians 30)))

(defparameter *particles* nil)
(dotimes (i 100)
  (push (make-instance 'particle :x (/ *width* 2)
                                 :y (/ *height* 2)
                                 :speed (+ (* (random 1.0) 4) 1)
                                 :direction (* (random 1.0) pi 2)) *particles*))


(defsketch ep8-velocity-position ((title "Episode 8 - Velocity & Position") (width *width*) (height *height*))
  (add! *position* *velocity*)
  (circle (get-x *position*) (get-y *position*) 20))

(defsketch ep8-particle ((title "Episode 8 - Particle") (width *width*) (height *height*))
  (update *particle*)
  (with-slots (position) *particle*
    (circle (get-x position) (get-y position) 20)))

(defsketch ep8-particles ((title "Episode 8 - Particles") (width *width*) (height *height*))
  (loop for particle in *particles*
        do (process-particle particle)))

(defun process-particle (particle)
  (update particle)
  (with-slots (position) particle
    (circle (get-x position) (get-y position) 20)))



(make-instance 'ep8-velocity-position)
(make-instance 'ep8-particle)
(make-instance 'ep8-particles)
