(defpackage :particle
  (:use :cl :vector)
  (:export :particle :update))
(in-package :particle)


(defclass particle ()
  ((position)
   (velocity)))

(defmethod initialize-instance :after ((p particle) &key x y speed direction)
  (setf (slot-value p 'position) (make-instance 'vector2 :x x :y y))
  (setf (slot-value p 'velocity) (make-instance 'vector2 :length speed :angle direction)))

(defmethod update ((p particle))
  (add! (slot-value p 'position) (slot-value p 'velocity)))
