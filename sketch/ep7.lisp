(defpackage :ep7
  (:use :cl :sketch)
  (:export :vector2
           :get-x
           :set-x
           :get-y
           :set-y
           :get-angle
           :set-angle
           :get-length
           :set-length))

(in-package :ep7)


(defclass vector2 ()
  ((x :initarg :x)
   (y :initarg :y)))

(defmethod get-x ((vec vector2))
  (slot-value vec 'x))

(defmethod set-x ((vec vector2) x)
  (setf (slot-value vec 'x) x))

(defmethod get-y ((vec vector2))
  (slot-value vec 'y))

(defmethod set-y ((vec vector2) y)
  (setf (slot-value vec 'y) y))

(defmethod get-angle ((vec vector2))
  (let ((x (get-x vec)) (y (get-y vec)))
    (atan y x)))

(defmethod set-angle ((vec vector2) angle)
  (let ((len (get-length vec)))
    (setf (slot-value vec 'x) (* (cos angle) len))
    (setf (slot-value vec 'y) (* (sin angle) len)))
  angle)

(defmethod get-length ((vec vector2))
  (let ((x (get-x vec)) (y (get-y vec)))
    (sqrt (+ (* x x) (* y y)))))

(defmethod set-length ((vec vector2) len)
  (let ((angle (get-angle vec)))
    (setf (slot-value vec 'x) (* (cos angle) len))
    (setf (slot-value vec 'y) (* (sin angle) len)))
  len)


; (defvar *vec* (make-instance 'vector2 :x 5 :y 1))
