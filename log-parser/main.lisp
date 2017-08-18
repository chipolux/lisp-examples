;;;; A system for parsing and recording data from access logs.

;;;; Log format:
;;;; {remote ip} - {user} [{time}] "{method} {uri} {protocol}" {status} {size}

;;;; {when} format:
;;;; DD/MMM/YYYY:hh:mm:ss +ZZZZ

(ql:quickload :cl-ppcre)
(ql:quickload :drakma)

;; groups: ip, user, when, method, uri, protocol, status, size
(defparameter *entry-regex* "(.*) - (.*) \\[(.*)\\] \"(.*) (.*) (.*)\" (.*) (.*)")
(defvar *ip-db* (make-hash-table :test #'equal))


(defun parse-log-entry (line)
  "Uses regex to parse interesting data a log entry."
  (cl-ppcre:register-groups-bind (ip nil when method uri nil status nil) (*entry-regex* line)
    (list :ip ip :when when :method method :uri uri :status status)))


(defun record-log-entry (entry)
  "Records an entry in the *ip-db* by ip."
  (let ((ip (getf entry :ip)))
    (push entry (gethash ip *ip-db*))))


(defun read-log-file (path)
  "Reads a log file, parses data from each line, and stores data into *ip-db*."
  (with-open-file (f path)
    (loop for line = (read-line f nil)
          while line
          do (record-log-entry (parse-log-entry line)))))


(defun ip-connection-count ()
  "Returns an alist of ips and the number of times they connected."
  (let ((alist nil))
    (maphash #'(lambda (k v) (push (cons k (length v)) alist)) *ip-db*)
    alist))


(defun hash-table-alist (table)
  "Converts a hash-table to an alist."
  (let ((alist nil))
    (maphash #'(lambda (k v) (push (cons k v) alist)) table)
    alist))


(defun top-n (alist n)
  "returns the top n elements of an alist sorted by cdr."
  (subseq (sort alist #'> :key #'cdr) 0 (min n (length alist))))


(defparameter *ip-api-url* "http://ip-api.com/line/~a?lang=en&fields=status,country,regionName,city,org")


(defun load-ip-info (ip)
  "Uses drakma to load info about an ip from ip-api.com."
  (let ((s (drakma:http-request (format nil *ip-api-url* ip) :want-stream t)))
    (if (equal (read-line s) "success")
      (list :country (read-line s) :region (read-line s) :city (read-line s) :org (read-line s)))))
