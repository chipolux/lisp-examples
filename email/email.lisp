;;;; A simple library to handle parsing emails according to RFC 5322
;;;; https://tools.ietf.org/html/rfc5322
(defpackage :email
  (:use :common-lisp)
  (:export
    :read-email-file
    :plaintext-p))

(in-package :email)


(defconstant +whitespace+ '(#\Space #\Tab #\Newline #\Return))

;;; It is safer to use iso-8859-1 when reading email files than the default
#+CLISP
(defconstant +encoding+ charset:iso-8859-1)
#+(or CCL SBCL)
(defconstant +encoding+ :iso-8859-1)


(defun strip (str)
  (string-trim +whitespace+ str))

(defun join (&rest args)
  (format nil "~{~a~^ ~}" (mapcar #'strip args)))

(defun foldedp (line)
  (find (char line 0) +whitespace+))

(defun header-char-p (c)
  (let ((code (char-code c)))
    (and (>= code 33) (<= code 126) (not (= code 58)))))

(defun get-header-entry (key header)
  (first (gethash key header)))

(defun add-header-entry (line header)
  (let* ((sep_pos (position #\: line)) (key (subseq line 0 sep_pos)))
    (if (every #'header-char-p key)
      (push
        (strip (subseq line (1+ sep_pos)))
        (gethash (strip key) header)))))

(defun read-header (f)
  (loop for line = (read-line f) until (equal line "") collecting line))

(defun unfold-header (lines)
  (let ((buff "") (new-lines nil))
    (loop for line in (reverse lines)
          if (foldedp line) do (setf buff (join line buff))
          else do (push (join line buff) new-lines)
          and do (setf buff ""))
    new-lines))

(defun parse-header (f)
  (let ((header (make-hash-table :test #'equal))
        (header-lines (unfold-header (read-header f))))
    (loop for line in header-lines do (add-header-entry line header))
    header))

(defun parse-body (f)
  (format nil "~{~a~^~%~}" (loop for line = (read-line f nil) while line collect line)))

(defun read-email-file (path)
  (with-open-file (f path :direction :input :external-format +encoding+)
    (values
      (parse-header f)
      (parse-body f))))

(defun plaintext-p (header)
  (search "text/plain" (get-header-entry "Content-Type" header)))
