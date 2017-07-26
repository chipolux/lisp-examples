(load "parser.lisp")

(or
  #+SBCL (sb-ext:save-lisp-and-die "sbcl-parser" :toplevel #'main :executable t)
  #+CCL (ccl:save-application "ccl-parser" :toplevel-function #'main :prepend-kernel t)
  (format t "Cannot compile on this platform!"))
