(defparameter *english-list*
  "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}~%")

(format t *english-list* '())
(format t *english-list* '(1 2))
(format t *english-list* '(1 2 3))
(format t *english-list* '(1 2 3 4))
