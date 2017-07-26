(ql:quickload :hunchentoot)

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080))

(hunchentoot:define-easy-handler (say-hello :uri "/hello") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hello, ~a!~%" name))
