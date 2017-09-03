(defpackage :ep2
  (:use :cl :sketch))

(in-package :ep2)

(defsketch ep2 ((title "Episode 2") (width 1280) (height 720))
  (translate 0 (/ height 2))
  (scale 1 -1)
  (with-pen (make-pen :fill +black+)
    (do ((angle 0.00 (+ angle 0.01))) ((> angle (* 2 pi)))
      (rect (* 200 angle) (* 200 (sin angle)) 5 5))))


(make-instance 'ep2)
