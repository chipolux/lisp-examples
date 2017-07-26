(defun cwd ()
  (or
    #+CLISP (ext:cd)
    #+CCL (ccl:current-directory)
    #+SBCL (truename ".")
    nil))

(defun main ()
  (map nil #'(lambda (path) (format t "~a~%" path)) (directory "*")))
