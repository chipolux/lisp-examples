(defpackage :particle
  (:use :cl :vector))
(in-package :particle)


(defclass particle ()
  ((position)
   (velocity)))

(defmethod initialize-instance :after ((p particle) x y speed direction)
  (setf (slot-value p 'position) (make-instance 'vector2 :x x :y y))
  (setf (slot-value p 'velocity) (make-instance 'vector2 :length speed :angle direction)))
