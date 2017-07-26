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

(defparameter *screen-width* 1280)
(defparameter *screen-height* 720)

(defun main (&key (delay 2000))
  (sdl2:with-init (:video)
    (sdl2:with-window (window :title "Some Window" :w *screen-width* :h *screen-height*)
      (let ((screen-surface (sdl2:get-window-surface window)))
        (sdl2:fill-rect screen-surface nil (sdl2:map-rgb (sdl2:surface-format screen-surface) 0 255 0))
        (sdl2:update-window window)
        (sdl2:delay delay))
      (sdl2:with-event-loop
        (:method :poll)
        (:idle () (sdl2:gl-swap-window window))
        (:quit () t)))))
