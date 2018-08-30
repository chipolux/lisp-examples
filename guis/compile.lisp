(load "opengl.lisp")

(or
  #+SBCL (sb-ext:save-lisp-and-die "sbcl-opengl" :toplevel #'run :executable t)
  #+CCL (ccl:save-application "ccl-opengl" :toplevel-function #'run :prepend-kernel t)
  (format t "Cannot compile on this platform!"))
