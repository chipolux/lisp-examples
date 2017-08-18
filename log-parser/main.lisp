;;;; A system for parsing and recording data from access logs.

;;;; Log format:
;;;; {remote ip} - {user} [{time}] "{method} {uri} {protocol}" {status} {size}

;;;; {when} format:
;;;; DD/MMM/YYYY:hh:mm:ss +ZZZZ

(ql:quickload :cl-ppcre)
(ql:quickload :drakma)


(defparameter *entry-regex* "(.*) - (.*) \\[(.*)\\] \"(.*)\"")
(defvar *ip-db* (make-hash-table :test #'equal))


(defun read-log-file (path)
  (with-open-file (f path)
    (loop for line = (read-line f nil)
          while line
          do (record-log-entry (parse-log-entry line)))))


(defun parse-log-entry (line)
  (cl-ppcre:register-groups-bind (ip nil when) (*entry-regex* line)
    (list :ip ip :when when)))


(defun record-log-entry (entry)
  (let* ((ip (getf entry :ip)) (times-seen (gethash ip *ip-db*)))
    (if times-seen
      (setf (gethash ip *ip-db*) (1+ times-seen))
      (setf (gethash ip *ip-db*) 1))))


(defun hash-table-alist (table)
  (let ((alist nil))
    (maphash #'(lambda (k v) (push (cons k v) alist)) table)
    alist))


(defun hash-table-top-n-values (table n)
  (subseq (sort (hash-table-alist table) #'> :key #'cdr) 0 n))


(defparameter *ip-api-url* "http://ip-api.com/line/~a\?lang\=en\&fields\=status,country,regionName,city,org")


(defun load-ip-info (ip)
  (let ((s (drakma:http-request (format nil *ip-api-url* ip) :want-stream t)))
    (if (equal (read-line s) "success")
      (list :country (read-line s) :region (read-line s) :city (read-line s) :org (read-line s)))))
