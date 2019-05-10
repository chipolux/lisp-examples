(defun parse-float (n)
  "Parse an IEEE-754 float from a 32bit integer. Nil if Infinity or NaN."
  (let ((sign-bit (ldb (byte 1 31) n))
        (exponent (ldb (byte 8 23) n))
        (mantissa (ldb (byte 23 0) n)))
    (if (< exponent 255)
      (* (expt -1 sign-bit) (1+ (/ mantissa (expt 2 23))) (expt 2 (- exponent 127)))
      nil)))

(defun read-float (s)
  "Read an IEEE-754 float (32bit) from the provided byte stream."
  (let ((raw 0))
    (loop for i upto 3 do (setf raw (dpb (read-byte s) (byte 8 (- 24 (* i 8))) raw)))
    (parse-float raw)))

(defun parse-gf (p)
  "Parse a gridfloat file into an array of points."
  (with-open-file (s p :direction :input :element-type '(unsigned-byte 8))
    (loop repeat 4 collect (read-float s))))
