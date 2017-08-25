(load "email.lisp")
(load "pathing.lisp")


(defun collect-plain-emails (dir)
  (let ((paths (pathing:list-directory dir)))
    (format t "Total Files: ~a~%" (length paths))
    (loop
      for path in paths
      for (header body) = (multiple-value-list (email:read-email-file path))
      when (email:plaintext-p header)
      collect body)))
