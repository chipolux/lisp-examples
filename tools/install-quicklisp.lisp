;;; if ~/.quicklisp doesn't exist, install quicklisp there
(unless (probe-file "~/.quicklisp")
  (load "quicklisp.lisp")
  (quicklisp-quickstart:install :path "~/.quicklisp"))

(ql:add-to-init-file)
