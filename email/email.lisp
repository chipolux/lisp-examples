(defpackage :email
  (:use :common-lisp)
  (:export
    :read-email-file
    :plaintext-p))

(in-package :email)


;;; It is safer to use iso-8859-1 when reading email files than the default
#+CLISP
(defconstant +encoding+ charset:iso-8859-1)
#+(or CCL SBCL)
(defconstant +encoding+ :iso-8859-1)


(defun read-email-file (path)
    (with-open-file (f path :direction :input :external-format +encoding+)
      (values
        (loop for line = (read-line f) until (equal line "") collect line)
        (loop for line = (read-line f nil) while line collect line))))


(defun plaintext-p (header) t)
