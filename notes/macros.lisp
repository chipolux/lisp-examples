;; brute force check if a number is a prime
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))


;; use our brute forcing function to find the next prime
(defun next-prime (number)
  (loop for n from number when (primep n) return n))


(defmacro once-only ((&rest names) &body body)
    (let ((gensyms (loop for n in names collect (gensym))))
          `(let (,@(loop for g in gensyms collect `(,g (gensym))))
                   `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
                              ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
                                            ,@body)))))


(defmacro with-gensyms ((prefix &rest syms) &body body)
  `(let ,(loop for sym in syms
               collect `(,sym (gensym ,(concatenate 'string prefix "-"))))
     ,@body))


(defmacro do-primes ((var start end) &body body)
  (with-gensyms ("DO-PRIMES-" end-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))) (,end-name ,end))
       ((> ,var ,end-name))
       ,@body)))


;; macro call
(macroexpand-1 '(do-primes (p 0 19) (format t "~d " p)))
(macroexpand-1 '(do-primes (p 0 (random 100)) (format t "~d " p)))
