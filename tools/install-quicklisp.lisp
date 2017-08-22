(load "quicklisp.lisp")

(handler-bind ((error #'(lambda (ex) (invoke-restart 'load-setup))))
  (quicklisp-quickstart:install :path "~/.quicklisp"))

(ql:add-to-init-file)
