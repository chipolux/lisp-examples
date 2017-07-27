(if (not (find-package :pathing)) (load "pathing.lisp"))

(defpackage :spam-filter
  (:use
    :common-lisp
    :pathing
    :cl-ppcre)
  (:export
    :clear-database
    :classify
    :train))

(in-package :spam-filter)


(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)

(defvar *feature-database* (make-hash-table :test #'equal))
(defvar *total-spams* 0)
(defvar *total-hams* 0)


(defun clear-database ()
  (setf *feature-database* (make-hash-table :test #'equal)
        *total-spams* 0
        *total-hams* 0))


(defun classify (text)
  (classification (score (extract-features text))))


(defun classification (score)
  "Classify a score (1.0 - 0.0) as ham, spam, or unsure."
  (values
    (cond
      ((<= score *max-ham-score*) :ham)
      ((>= score *min-spam-score*) :spam)
      (t :unsure))
    score))


(defclass word-feature ()
  ((word
     :initarg :word
     :accessor word
     :initform (error "Must supply :word!")
     :documentation "The word this feature represents.")
   (spam-count
     :initarg :spam-count
     :accessor spam-count
     :initform 0
     :documentation "The number of times this word appeard in spam.")
   (ham-count
     :initarg :ham-count
     :accessor ham-count
     :initform 0
     :documentation "The number of times this word appeard in ham.")))


(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word ham-count spam-count) object
      (format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))


(defun store-feature (word)
  "Store a word as a feature and return it, or return the existing feature."
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
            (make-instance 'word-feature :word word))))


(defun extract-words (text)
  "Use CL-PPCRE to extract all words longer than 3 characters."
  (delete-duplicates
    (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" (string-downcase text))
    :test #'string=))


(defun extract-features (text)
  "Get all features from some text, also store those features in the database."
  (mapcar #'store-feature (extract-words text)))


(defun train (text type)
  (dolist (feature (extract-features text))
    (increment-count feature type))
  (increment-total-count type))


(defun increment-count (feature type)
  (ecase type
    (:ham (incf (ham-count feature)))
    (:spam (incf (spam-count feature)))))


(defun increment-total-count (type)
  (ecase type
    (:ham (incf *total-hams*))
    (:spam (incf *total-spams*))))


;; calculate probability that a feature is spam like
;; sf / (sf + hf)
;; where:
;;   sf = s / ts
;;   s = number spams containing this feature
;;   ts = total spams
;;   hf = h / th
;;   h = number hams containing this feature
;;   th = total hams
(defun spam-probability (feature)
  (with-slots (spam-count ham-count) feature
    (let ((spam-freq (/ spam-count (max 1 *total-spams*)))
          (ham-freq (/ ham-count (max 1 *total-hams*))))
      (/ spam-freq (+ spam-freq ham-freq)))))


;; bp = spam-probability
;; p = s + h
;; w = 1
;; ap = 1/2
;; ((w * ap) + (p * bp)) / (w + p)
(defun bayesian-spam-probability (feature &optional (assumed-probability 1/2) (weight 1))
  (let ((basic-probability (spam-probability feature))
        (data-points (+ (spam-count feature) (ham-count feature))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))


(defun score (features)
  (let ((spam-probs ()) (ham-probs ()) (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
        (let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
          (push spam-prob spam-probs)
          (push (- 1.0d0 spam-prob) ham-probs)
          (incf number-of-probs))))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))


(defun untrained-p (feature)
  (with-slots (spam-count ham-count) feature
    (and (zerop spam-count) (zerop ham-count))))


(defun fisher (probs number-of-probs)
  (inverse-chi-square
    (* -2 (reduce #'+ probs :key #'log))
    (* 2 number-of-probs)))


(defun inverse-chi-square (value degrees-of-freedom)
  (assert (evenp degrees-of-freedom))
  (min
    (loop with m = (/ value 2)
          for i below (/ degrees-of-freedom 2)
          for prob = (exp (- m)) then (* prob (/ m i))
          summing prob)
    1.0))
