(defpackage :ep7
  (:use :cl :vector))
(in-package :ep7)

;;; Actual vector class definition has been moved to vector.lisp!

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
