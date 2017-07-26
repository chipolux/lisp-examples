(defvar *db* nil)


(defun make-cd (title artist rating ripped)
  "returns a cd structure filled with the provided data"
  (list :title title :artist artist :rating rating :ripped ripped))


(defun add-record (cd)
  "adds a cd to the *db*"
  (push cd *db*))


(defun dump-db ()
  "write *db* out to console"
  (dolist (cd *db*)
    (format *query-io* "~{~a:~10t~a~%~}~%" cd)))


(defun prompt-read (prompt)
  "prompt user for a line of input"
  (format *query-io* "~a: " prompt)
  (read-line *query-io*))


(defun prompt-for-cd ()
  "returns a cd structure created by prompting user for input"
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    (y-or-n-p "Ripped?")))


(defun add-cds ()
  "continuously prompts the user for cd info and adds them to the *db*"
  (loop
    (add-record (prompt-for-cd))
    (if (not (y-or-n-p "Another?")) (return))))


(defun save-db (filename)
  "write *db* out to provided filename"
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))


(defun load-db (filename)
  "load *db* from provided filename"
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))


(defun select (selector-fn)
  "apply provided selector function to the *db*"
  (remove-if-not selector-fn *db*))


(defun delete-rows (selector-fn)
  "deletes rows matching selector function"
  (setf *db* (remove-if selector-fn *db*)))


(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  "updates rows matching selector function"
  (setf *db*
        (map 'list #'(lambda (row)
                       (when (funcall selector-fn row)
                         (if title    (setf (getf row :title)  title))
                         (if artist   (setf (getf row :artist) artist))
                         (if rating   (setf (getf row :rating) rating))
                         (if ripped-p (setf (getf row :ripped) ripped)))
                       row) *db*)))


; a backtick (`) is a special variant of the single quote (') it stops an
; expression from being evaluated but let's you selectively evaluate parts of
; the expression by preceding them with a comma (,)
(defun make-comparison-expr (field value)
  "convert a :field and value into a comparison expression"
  `(equal (getf cd ,field) ,value))


(defun make-comparisons-list (fields)
  "take a list of :field value pairs and generate comparison expressions for them"
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))


; the comma-at (,@) splices the resulting evaluated expression (which must return a list)
; into it's place in the expression rather then embedding the result as a list
(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))


;; old 'where' that was unflexible
; (defun where (&key title artist rating (ripped nil ripped-p))
;   "return selector function that filters according to provided keywords"
;   #'(lambda (cd)
;       (and
;         (if title    (equal (getf cd :title)  title)  t)
;         (if artist   (equal (getf cd :artist) artist) t)
;         (if rating   (equal (getf cd :rating) rating) t)
;         (if ripped-p (equal (getf cd :ripped) ripped) t))))
