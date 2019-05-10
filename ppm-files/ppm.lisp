(defun write-header (s width height depth)
  (write-sequence
    (loop for c across
          (format nil "P6 ~s ~s ~s~%" width height depth)
          collect (char-code c))
    s)
  t)


(defun gen-pixel (x y width height depth)
  (list
    (floor (* (/ x width) depth))
    (floor (* (/ (+ x y) (+ width height)) depth))
    (floor (* (/ y height) depth))))


(defun write-pixels (s width height depth)
  (write-sequence
    (loop for i upto (* width height)
          append (gen-pixel (mod i width) (/ i width) width height depth))
    s)
  t)


(defun test-ppm ()
  (with-open-file (s "test.ppm"
                     :direction :output
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
    (write-header s 800 800 255)
    (write-pixels s 800 800 255)))


(test-ppm)
