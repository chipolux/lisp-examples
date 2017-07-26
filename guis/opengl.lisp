;; Load required libraries.
(require :sdl2)
(require :cl-opengl)

(defun debug-log (msg &rest args)
  "Output and flush MSG to STDOUT with arguments ARGS"
  (apply #'format t msg args)
  ;; Flush to standard out
  (finish-output))
 
(defun main ()
  "The entry point of our game."
  (sdl2:with-init
    (:everything)
    (debug-log "Using SDL library version: ~D.~D.~D~%"
               sdl2-ffi:+sdl-major-version+
               sdl2-ffi:+sdl-minor-version+
               sdl2-ffi:+sdl-patchlevel+)

    (sdl2:with-window
      (win :flags '(:shown :opengl))
      (sdl2:with-gl-context
        (gl-context win)
        ;; Basic window/gl setup
        (setup-gl win gl-context)

        ;; Run main loop
        (main-loop win #'render)))))

(defun setup-gl (win gl-context)
  "Setup OpenGL with the window WIN and the gl context of GL-CONTEXT"
  (debug-log "Setting up window/gl.~%")
  (sdl2:gl-make-current win gl-context)
  (gl:viewport 0 0 800 600)
  (gl:matrix-mode :projection)
  (gl:ortho -2 2 -2 2 -2 2)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  ;; Clear to black
  (gl:clear-color 0.0 0.0 0.0 1.0))

(defun render ()
  (gl:clear :color-buffer)
  ;; Draw a demo triangle
  (gl:begin :triangles)
  (gl:color 1.0 0.0 0.0)
  (gl:vertex 0.0 1.0)
  (gl:vertex -1.0 -1.0)
  (gl:vertex 1.0 -1.0)
  (gl:end)
  (gl:flush))

(defun main-loop (win render-fn)
  "Run the game loop that handles input, rendering through the
  render function RENDER-FN, amongst others."
  (sdl2:with-event-loop
    (:method :poll)
    (:idle ()
           (funcall render-fn)
           ;; Swap back buffer
           (sdl2:gl-swap-window win))
    (:quit () t)))
