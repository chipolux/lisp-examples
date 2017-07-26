(defpackage :nakyle-com
 (:use :cl :wookie :wookie-plugin-export))
(in-package :nakyle-com)

;; load wookies core plugins
(load-plugins)


(defroute (:get "/") (req res)
  (send-response res :body "Welcome to nakyle.com!"))


(as:with-event-loop ()
  (let* ((listener (make-instance 'listener :bind nil :port 8080)))
    (as:signal=handler 2
      (lambda (sig)
        (declare (ignore sig))
        (as:free-signal-handler 2)
        (as:close-tcp-server server)))))
