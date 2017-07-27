;; OOP stuff (generic functions, classes, etc.)

;; This is the verbose way of creating a bank account with accessors, writers.
; (defclass bank-account ()
;   ((customer-name
;      :initarg :customer-name
;      :initform (error "Must supply customer name!"))
;    (balance
;      :initarg :balance
;      :initform 0)
;    (account-number
;      :initform (incf *account-numbers*))
;    account-type))

; (defgeneric balance (account))
; (defmethod balance ((account bank-account))
;   (slot-value account 'balance))

; (defgeneric (setf customer-name) (name account))
; (defmethod (setf customer-name) (name (account bank-account))
;   (setf (slot-value account 'customer-name) name))

; (defgeneric customer-name (account))
; (defmethod customer-name ((account bank-account))
;   (slot-value account 'customer-name))
(defvar *account-numbers* 0)
(defparameter *minimum-balance* 50)


;; inheritance rules for slots:
;;   :initform, :allocation work like inheritance from Python
;;   :initargs is combined so all the old args still work
;;   :reader, :writer, :accessor don't get copied, but the methods created by
;;     the parent class(es) will apply to the child unless you provide new ones
;;     specialized on the new class
;; with multiple inheritance the parents take precendence from first listed to
;; last, this affects basically all the slot merging and generic method stuff.
(defclass bank-account ()
  ((customer-name
     :initarg :customer-name
     :initform (error "Must supply customer name!")
     :accessor customer-name
     :documentation "Customer's Name")
   (balance
     :initarg :balance
     :initform 0
     :reader balance
     :documentation "Account Balance")
   (account-number
     :initform (incf *account-numbers*)
     :reader account-number
     :documentation "Account's Unique Number")
   (account-type
     :reader account-type
     :documentation "Account's Type (:gold :silver or :bronze)")))


;; the print-object method is used to display objects on the REPL
;; we use 'print-unreadable-object' to handle adding the implementation
;; specific unreadable object markers around what we print.
(defmethod print-object ((account bank-account) stream)
  (print-unreadable-object (account stream :type t)
    (with-slots (customer-name account-number account-type balance) account
      (format stream "~s, #~d, ~s, $~d" customer-name account-number account-type balance))))


;; initialize-instance is effectively equivilant to __init__ in Python
;; the :after is important so that the class has all it's slots filled
(defmethod initialize-instance :after ((account bank-account) &key)
  (let ((balance (balance account)))
    (setf (slot-value account 'account-type)
          (cond
            ((>= balance 100000) :gold)
            ((>= balance 50000) :silver)
            (t :bronze)))))


;; a defmethod will automatically do the defgeneric work for you if it isn't
;; already done.
(defmethod assess-low-balance-penalty ((account bank-account))
  (with-slots (balance) account
    (when (< balance *minimum-balance*)
      (decf balance (* balance .01)))))


(defvar *acnt1* (make-instance 'bank-account :customer-name "Bringle Papper" :balance 100))
(defvar *acnt2* (make-instance 'bank-account :customer-name "Zimmy Karsfer" :balance 51000))
(defvar *acnt3* (make-instance 'bank-account :customer-name "Zing Zing" :balance 200000))
