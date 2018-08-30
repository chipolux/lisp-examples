(require :sdl2)

; (defun old-main ()
;   (sdl2:with-init nil
;     (sdl2:with-window (win :flags '(:shown))640 480 :bpp 8 :sw t :double-buffer t)
;     (sdl:set-caption "SDL Test!" "Sdl testing")
;     (sdl:clear-display sdl:*blue*)
;     (let ((tile (sdl:load-image "art/64x74_blue.png")))
;       ;; (sdl:blit-surface tile))
;       (sdl:draw-surface-at tile (sdl:point :x 30 :y 40)))
;     (sdl:update-display)
;     (event-loop)))

; (defun event-loop ()
;   (sdl:with-events (:wait)
;     (:quit-event () t)
;     (:key-up-event () (sdl:push-quit-event))))

(defparameter *screen-width* 800)
(defparameter *screen-height* 600)

(defun format-now (msg &rest args)
  (apply #'format t msg args)
  (finish-output))

(defun main ()
  (sdl2:with-init (:everything)
    (format-now "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (sdl2:with-window (window :title "Some Window" :w *screen-width* :h *screen-height*)
      ; (let ((screen-surface (sdl2:get-window-surface window)))
      ;   (sdl2:fill-rect screen-surface nil (sdl2:map-rgb (sdl2:surface-format screen-surface) 0 255 0))
      ;   (sdl2:update-window window)
      ;   (sdl2:delay delay))
      (format-now "Beginning main loop.~%")
      (sdl2:with-event-loop (:method :poll)
        (:keyup (:keysym keysym)  ; get keyup events with the key symbol as keysym
          (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
            (format-now "Pushing quit event.~%")
            (sdl2:push-event :quit)))
        (:idle () (sdl2:gl-swap-window window))  ; on idle swap the window framebuffer
        (:quit () t)))))
