(defpackage :ep7
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
           :subtract
           :multiply
           :divide))

(in-package :ep7)


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

(defmethod subtract ((vec1 vector2) vec2)
  (let ((x1 (get-x vec1)) (y1 (get-y vec1)) (x2 (get-x vec2)) (y2 (get-y vec2)))
    (make-instance 'vector2 :x (- x1 x2) :y (- y1 y2))))

(defmethod multiply ((vec vector2) val)
  (let ((x (get-x vec)) (y (get-y vec)))
    (make-instance 'vector2 :x (* x val) :y (* y val))))

(defmethod divide ((vec vector2) val)
  (let ((x (get-x vec)) (y (get-y vec)))
    (make-instance 'vector2 :x (/ x val) :y (/ y val))))


;;; Video Examples

(defmethod show ((vec vector2))
  (format t "X: ~a~%" (get-x vec))
  (format t "Y: ~a~%" (get-y vec))
  (format t "Angle: ~a~%" (get-angle vec))
  (format t "Length: ~a~%" (get-length vec)))

(format t "Initial Vector 10, 5:~%")
(let ((vec (make-instance 'vector2 :x 10 :y 5)))
  (show vec))

(format t "~%Vector, Angle pi / 6, Length 100:~%")
(let ((vec (make-instance 'vector2 :x 10 :y 5)))
  (set-angle vec (/ pi 6))
  (set-length vec 100)
  (show vec))

(format t "~%Vector Addition (10, 5) + (3, 4):~%")
(let* ((vec1 (make-instance 'vector2 :x 10 :y 5))
       (vec2 (make-instance 'vector2 :x 3 :y 4))
       (vec3 (add vec1 vec2)))
  (show vec3))

(format t "~%Vector Multiplication (10, 5) * 2:~%")
(let* ((vec1 (make-instance 'vector2 :x 10 :y 5))
       (vec2 (multiply vec1 2)))
  (format t "Length 1: ~a~%" (get-length vec1))
  (format t "Length 2: ~a~%" (get-length vec2)))

(format t "~%Vector, Angle pi / 6, Length 3:~%")
(let ((vec (make-instance 'vector2 :angle (/ pi 6) :length 3)))
  (show vec))
