;; conditions are a special type of class that does not have to inherit from
;; `standard-object` so things like `slot-value` and `make-instance` don't work
;; for them. (some implementations do subclass `standard-object` but it's not
;; portable to rely on that.

;; when using a condition for error handling you should subclass `error`.

(define-condition malformed-log-entry-error (error)
  ((text :initarg :text :reader text)))


(defun starts-with-p (str substr)
  (let ((pos (search substr str)))
    (and pos (= pos 0))))

(defun well-formed-log-entry-p (text)
  (starts-with-p text "LOG"))

(defun parse-log-entry (text)
  (if (well-formed-log-entry-p text)
    ; (make-instance 'log-entry text)
    text
    (restart-case (error 'malformed-log-entry-error :text text)
      (use-value (value) value)
      (reparse-entry (fixed-text) (parse-log-entry fixed-text)))))

(defun parse-log-file (file)
  (with-open-file (in file :direction :input)
    (loop for text = (read-line in nil nil) while text
       for entry = (restart-case (parse-log-entry text)
                     (skip-log-entry () nil))
       when entry collect it)))

(defun skip-log-entry (c)
  (let ((r (find-restart 'skip-log-entry)))
    (when r (invoke-restart r))))

(defparameter *log-files* '("test.log"))

;; use the skip-log-entry restart
; (defun log-analyzer ()
;   (handler-bind ((malformed-log-entry-error #'skip-log-entry))
;     (dolist (log *log-files*)
;       (analyze-log log))))

;; use the use-value restart instead with "BAD LOG"
(defun log-analyzer ()
  (handler-bind ((malformed-log-entry-error #'(lambda (c) (use-value "BAD LOG!"))))
    (dolist (log *log-files*)
      (analyze-log log))))


(defun analyze-log (log)
  (dolist (entry (parse-log-file log))
    (format t "~a: ~a~%" log entry)))
