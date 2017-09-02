;;;; tutorial code from https://github.com/vydd/sketch
(ql:quickload :sketch)

(defpackage :tut
  (:use :cl :sketch)
  (:export :run))

(in-package :tut)

(defsketch tut-1 ((title "A Window")))

(defsketch tut-2 ((title "Rectangle"))
  (rect 100 100 200 200))

(defsketch tut-3 ((title "Rectangles"))
  (dotimes (i 10)
    (rect (* i 40) (* i 40) 40  40)))

(defsketch tut-4 ((title "Stairs And A Circle"))
  (dotimes (i 10)
    (rect 0 (* i 40) (* (+ i 1) 40) 40))
  (circle 300 100 50))

(defsketch tut-5 ((title "X"))
  (line 0 0 400 400)
  (line 400 0 0 400))

(defsketch tut-6 ((title "Empty Downards Chevron"))
  (polyline 100 100
            200 150
            300 100
            200 200
            100 100))

(defsketch tut-7 ((title "Full Downwards Chevron"))
  (polygon 100 100
           200 150
           300 100
           200 200))

(defsketch tut-8 ((title "Growing Sides"))
  (dotimes (i 4)
    (ngon (+ i 3)           ; number of sides
          (+ 50 (* i 100))  ; center x
          200               ; center y
          20                ; radius x
          20                ; radius y
          (* i 20))))       ; angle

(defsketch tut-9 ((title "Lerping Colors"))
  (dotimes (i 4)
    (with-pen (make-pen :fill (lerp-color +red+ +yellow+ (/ i 4)))
      (rect (* i 100) 0 100 200))))

(defparameter *colors* (loop for i below 16 collect (random-color)))
(defsketch tut-10 ((title "Random Colors"))
  (dotimes (x 8)
    (dotimes (y 2)
      (with-pen (make-pen :fill (elt *colors* (+ x (* y 8))))
        (rect (* x 50) (* y 50) 50 50)))))

(defsketch tut-11 ((title "Hashed Colors"))
  (dotimes (i 128)
    (with-pen (make-pen :fill (hash-color i))
      (rect (* i (/ 400 128)) 0 (/ 400 128) 100))))

(defsketch tut-12 ((title "Fade To Gray"))
  (dotimes (i 10)
    (let ((color (hash-color i)))
      (with-pen (make-pen :fill (color-filter-grayscale color :luminosity))  ; the default
        (rect (* i 40) 0 40 100))
      (with-pen (make-pen :fill color)
        (rect (* i 40) 100 40 100))
      (with-pen (make-pen :fill (color-filter-grayscale color :average))
        (rect (* i 40) 200 40 100)))))

(defsketch tut-13 ((title "Inversion") (width 300) (height 300) (i 0))
  (background +white+)
  (incf i 0.01)
  (let ((color (rgb (abs (sin i)) (abs (cos i)) 0)))
    (with-pen (make-pen :fill color)
      (circle 100 150 50))
    (with-pen (make-pen :fill (color-filter-invert color))
      (circle 200 150 50))))

(defsketch tut-14 ((title "Rotation") (width 300) (height 300) (i 0) (color (rgb 0.2 0.8 1.0)))
  (background +white+)
  (incf i 1)
  (when (zerop (mod i 60))
    (setf color (color-filter-rotate color)))
  (with-pen (make-pen :fill color)
    (rect 100 100 100 100)))

(defsketch tut-15 ((title "Shifting Hues") (width 400) (height 300) (color (rgb 0.2 0.5 0.6)))
  (dotimes (i 4)
    (with-pen (make-pen :fill (color-filter-hsb color :hue (* 0.1 (+ i 1))))
      (rect (* i 100) 0 100 100))
    (with-pen (make-pen :fill (color-filter-hsb color :saturation (* 0.1 (+ i 1))))
      (rect (* i 100) 100 100 100))
    (with-pen (make-pen :fill (color-filter-hsb color :brightness (* 0.1 (+ i 1))))
      (rect (* i 100) 200 100 100))))

(defsketch tut-16 ((title "Black And Blue"))
  (with-pen (make-pen :fill +black+)
    (rect 100 100 100 100))
  (with-pen (make-pen :fill +blue+)
    (circle 315 315 50)))

(defsketch tut-17 ((title "Stroking"))
  (with-pen (make-pen :stroke +red+)
    (rect 100 100 100 100)))

(defsketch tut-18 ((title "Full Stroking"))
  (with-pen (make-pen :stroke (rgb 0.5 0.0 0.6) :fill (rgb 0.0 0.8 0.8))
    (rect 50 50 100 75)
    (circle 300 220 100)))

(defsketch tut-19 ((title "The Stroke Thickens"))
  (dotimes (i 10)
    (with-pen (make-pen :stroke +white+ :weight (+ i 1))  ; weight can't be zero
      (line 50 (* i 20) 350 (* i 20)))))

(defsketch tut-20 ((title "Smoothing Out The Curve"))
  (dotimes (i 99)
    (with-pen (make-pen :stroke +red+ :curve-steps (+ i 1))  ; curve steps increase line segments for things like bezier
      (bezier 0 400
              100 100  
              300 100
              400 400))))


(defun run () (make-instance 'tut-20))
