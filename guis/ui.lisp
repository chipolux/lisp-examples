(require :sdl2)

(defparameter *elements* '())

(defclass element ()
  ((x :initarg :x
      :initform 0)
   (y :initarg :y
      :initform 0)
   (width :initarg :width
      :initform 0)
   (height :initarg :height
      :initform 0)
   (color :initarg :color
      :initform '(255 140 0 255))
   (hover-color :initarg hover-color
       :initform '(196 2 51 255))
   (pressed-color :initarg :pressed-color
       :initform '(196 2 51 255))))

(defmethod draw ((e element) r mx my mb)
  (with-slots (x y width height color hover-color) e
    (if (and (>= mx x)
             (>= my y)
             (<= mx (+ x width))
             (<= my (+ y height)))
      (apply #'sdl2:set-render-draw-color r hover-color)
      (apply #'sdl2:set-render-draw-color r color))
    (sdl2:render-fill-rect r (sdl2:make-rect x y width height))))

(defun clear (renderer &key (color '(105 105 105 255)))
  (apply #'sdl2:set-render-draw-color renderer color)
  (sdl2:render-clear renderer))

(defun process-elements (r)
  (multiple-value-bind (mx my mb) (sdl2:mouse-state)
    (loop for e in *elements* do (draw e r mx my mb))))

(defun ui ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "UI Test" :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
        (sdl2:with-event-loop (:method :poll)
          (:keyup (:keysym keysym)
            (case (sdl2:scancode keysym)
              (:scancode-escape (sdl2:push-event :quit))))
          (:idle ()
            (clear renderer)
            (process-elements renderer)
            (sdl2:render-present renderer)
            (sdl2:delay 33))
          (:quit () t))))))

(defun run ()
  (push (make-instance 'element :x 10 :y 10 :width 50 :height 50) *elements*)
  (push (make-instance 'element :x 70 :y 10 :width 50 :height 50) *elements*)
  #-sbcl (ui)
  #+sbcl (sdl2:make-this-thread-main #'ui))
