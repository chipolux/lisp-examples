(load "ls.lisp")

(defun app-name ()
  (or
    #+(and SBCL WIN32) "sbcl-test.exe"
    #+(and CCL WINDOWS) "ccl-test.exe"
    #+SBCL "sbcl-test"
    #+CCL "ccl-test"))

(or
  #+SBCL (sb-ext:save-lisp-and-die (app-name) :toplevel #'main :executable t)
  #+CCL (ccl:save-application (app-name) :toplevel-function #'main :prepend-kernel t)
  (format t "Cannot compile on this platform!"))
