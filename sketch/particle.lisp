(defpackage :particle
  (:use :cl :vector)
  (:export :particle :update :accelerate :set-position :set-velocity))
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

(defmethod set-position ((p particle) x y)
  (set-x (slot-value p 'position) x)
  (set-y (slot-value p 'position) y))

(defmethod set-velocity ((p particle) speed direction)
  (set-length (slot-value p 'velocity) speed)
  (set-angle (slot-value p 'velocity) direction))
