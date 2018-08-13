;;; if ~/.quicklisp doesn't exist, install quicklisp there
(load "quicklisp.lisp")

(unless (probe-file "~/.quicklisp")
  (format t "Installing quicklisp...")
  (quicklisp-quickstart:install :path "~/.quicklisp"))

(format t "Adding quicklisp to init file...")
(ql:add-to-init-file)
