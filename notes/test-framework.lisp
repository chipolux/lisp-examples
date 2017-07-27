(defvar *test-name* nil) ; a list of the names of the currently running tests


;; with-gensyms is built into CLISP
#-CLISP
(defmacro with-gensyms ((prefix &rest syms) &body body)
  `(let ,(loop for sym in syms
               collect `(,sym (gensym ,(concatenate 'string prefix "-"))))
     ,@body))


(defmacro combine-results (&body forms)
  "Combines the results (as booleans) of evaluating `forms` in order."
  (with-gensyms ("COMBINE-RESULTS" result)
    `(let ((,result t))
       ,@(loop for form in forms collect `(unless ,form (setf ,result nil)))
       ,result)))


(defun report-result (result form)
  "Reports the results of a single test case, usually called by check."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)


(defmacro check (&body forms)
  "Runs each expression in forms as a test case."
  `(combine-results
     ,@(loop for form in forms collect `(report-result ,form ',form))))


(defmacro deftest (name parameters &body body)
  "Defines a test function. Within a test function we can call other test
  functions or `check` to run individual test cases."
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))


(deftest test-+ ()
  (check (= (+ 1 2) 3)
         (= (+ 1 2 3) 6)
         (not (= (+ 0 0) 5))
         (= (+ 0 0) 0)
         (= (+ -1 -2) -3)))


(deftest test-* ()
  (check (= (* 4 4) 16)
         (= (* 4 -4) -16)
         (= (* 0 5) 0)))


(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))
