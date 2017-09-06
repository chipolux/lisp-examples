(defpackage :ep8
  (:use :cl :sketch :ep7))
(in-package :ep8)

(defparameter *width* 400)
(defparameter *height* *width*)
(defparameter *position* (make-instance 'vector2 :x 100 :y 100))
(defparameter *velocity* (make-instance 'vector2 :length 3 :angle (radians 30)))
;; note: in the video they use pi / 6, which is ~0.5 radians, or 30 degrees.
;;       sketch provides the radians and degrees conversion functions so I just
;;       use those here instead of the pi math.


(defsketch ep8 ((title "Episode 8") (width *width*) (height *height*))
  (add! *position* *velocity*)
  (circle (get-x *position*) (get-y *position*) 20))


(make-instance 'ep8)
