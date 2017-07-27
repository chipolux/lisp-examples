(load "email.lisp")
(load "pathing.lisp")


(defun collect-plain-emails (dir)
  (loop
    for path in (pathing:list-directory dir)
    for (header body) = (multiple-value-list (email:read-email-file path))
    when (email:plaintext-p header)
    collect body))
