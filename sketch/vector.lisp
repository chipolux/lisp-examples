(defpackage :vector
  (:use :cl)
  (:export :vector2
           :get-x
           :set-x
           :get-y
           :set-y
           :get-angle
           :set-angle
           :get-length
           :set-length
           :add
           :add!
           :subtract
           :multiply
           :divide
           :distance))

(in-package :vector)


(defclass vector2 ()
  ((x :initarg :x
      :initform 0)
   (y :initarg :y
      :initform 0)))

(defmethod initialize-instance :after ((vec vector2) &key length angle)
  (if length (set-length vec length))
  (if angle (set-angle vec angle)))

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

(defmethod add ((vec1 vector2) vec2)
  (let ((x1 (get-x vec1)) (y1 (get-y vec1)) (x2 (get-x vec2)) (y2 (get-y vec2)))
    (make-instance 'vector2 :x (+ x1 x2) :y (+ y1 y2))))

(defmethod add! ((vec1 vector2) vec2)
  (let ((x1 (get-x vec1)) (y1 (get-y vec1)) (x2 (get-x vec2)) (y2 (get-y vec2)))
    (set-x vec1 (+ x1 x2))
    (set-y vec1 (+ y1 y2)))
  vec1)

(defmethod subtract ((vec1 vector2) vec2)
  (let ((x1 (get-x vec1)) (y1 (get-y vec1)) (x2 (get-x vec2)) (y2 (get-y vec2)))
    (make-instance 'vector2 :x (- x1 x2) :y (- y1 y2))))

(defmethod multiply ((vec vector2) val)
  (let ((x (get-x vec)) (y (get-y vec)))
    (make-instance 'vector2 :x (* x val) :y (* y val))))

(defmethod divide ((vec vector2) val)
  (let ((x (get-x vec)) (y (get-y vec)))
    (make-instance 'vector2 :x (/ x val) :y (/ y val))))

(defmethod distance ((vec1 vector2) (vec2 vector2))
  (let ((x1 (get-x vec1)) (y1 (get-y vec1)) (x2 (get-x vec2)) (y2 (get-y vec2)))
    (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))
