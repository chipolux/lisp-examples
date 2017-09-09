(defpackage :particle
  (:use :cl :vector)
  (:export :particle :update :accelerate))
(in-package :particle)


(defclass particle ()
  ((position)
   (velocity)
   (gravity)))

(defmethod initialize-instance :after ((p particle) &key x y speed direction gravity)
  (setf (slot-value p 'position) (make-instance 'vector2 :x x :y y))
  (setf (slot-value p 'velocity) (make-instance 'vector2 :length speed :angle direction))
  (if gravity
    (setf (slot-value p 'gravity) (make-instance 'vector2 :x 0 :y gravity))
    (setf (slot-value p 'gravity) (make-instance 'vector2 :x 0 :y 0))))

(defmethod update ((p particle))
  (add! (slot-value p 'velocity) (slot-value p 'gravity))
  (add! (slot-value p 'position) (slot-value p 'velocity)))

(defmethod accelerate ((p particle) accel)
  (add! (slot-value p 'velocity) accel))
