(defun graphic-code-p (c)
  "Determine if a code point is printable."
  (if (graphic-char-p (code-char c)) t))

(defun code-string (c)
  "Convert a code point into a string."
  (string (code-char c)))

(defun parse-byte (b)
  "Return a string if a byte is printable, it's value otherwise."
  (if (graphic-code-p b) (code-string b) b))

(defmacro while (condition &body body)
  `(loop while ,condition do (progn ,@body)))

(defun parse-file (path)
  "Print a files printable bytes."
  (with-open-file (s path :element-type `unsigned-byte)
    (loop
      for b = (read-byte s nil 'eof) while (not (eq b 'eof))
      do (format t "~a~%" (parse-byte b)))))

(defun command-line ()
  (or
    #+CLISP *args*
    #+SBCL (rest sb-ext:*posix-argv*)
    #+CCL (rest ccl:*command-line-argument-list*)
    nil))

(defun main ()
  "Entrypoint function."
  (let ((input-file (car (last (command-line)))))
    (if input-file (parse-file input-file))))
