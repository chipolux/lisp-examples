;;; A Simple MODBUS TCP Server and Client Implementation

(defpackage :mb
  (:use :common-lisp)
  (:export
    :bytes->int
    :int->bytes
    :int->bits
    :make-server
    :read-registers
    :read-coils))
(in-package :mb)

(defparameter *t-id* 0)
(defparameter *max-connections* 5)
(defparameter *connections* '())

(defun bytes->int (bytes)
  "Converts a list of bytes into an integer. (big-endian)"
  (let ((v 0))
    (loop for b in bytes
          for p from (* 8 (1- (length bytes))) downto 0 by 8
          do (setf v (dpb b (byte 8 p) v)))
    v))

(defun int->bytes (size int)
  "Converts an integer into a list of bytes. (big-endian)"
  (loop for p from (* 8 (1- size)) downto 0 by 8 collect (ldb (byte 8 p) int)))

(defun int->bits (size int)
  "Converts an integer into a list of bits."
  (loop for p from 0 upto (1- size) collect (ldb (byte 1 p) int)))

(defun read-int (size stream)
  "Reads an integer from 'size' bytes of the provided stream. (big-endian)"
  (bytes->int (loop repeat size collect (read-byte stream))))

(defun write-int (size int stream)
  "Writes an integer as 'size' bytes to the provided stream. (big-endian)"
  (loop for b in (int->bytes size int) do (write-byte b stream)))

(defun read-mbap (stream)
  "Reads the ModBus Application Protocol header from provided stream."
  (values
    (read-int 2 stream)       ; transaction id
    (read-int 2 stream)       ; protocol id (always 0)
    (- (read-int 2 stream) 2) ; size in bytes (- unit id and function code)
    (read-byte stream)        ; unit id
    (read-byte stream)))      ; function code

(defun write-mbap (stream transaction-id protocol-id size unit-id function-code)
  "Writes the ModBus Application Protocol header to the provided stream."
  (if transaction-id
    (write-int 2 transaction-id stream)           ; use provided transaction id
    (progn (setf *t-id* (mod (1+ *t-id*) #xFFFF)) ; increment transaction id
           (write-int 2 *t-id* stream)))          ; use global transaction id
  (write-int 2 protocol-id stream)                ; protocol id (always 0)
  (write-int 2 (+ 2 size) stream)                 ; size in bytes (+ unit id and function code)
  (write-byte unit-id stream)                     ; unit id
  (write-byte function-code stream))              ; function code

(defun handle-client (c)
  "Handle a modbus client connection stream."
  (let ((s (sb-bsd-sockets:socket-make-stream c :input t :output t :element-type :default)))
    (handler-case
      (loop for (t-id p-id size u-id f-code) = (multiple-value-list (read-mbap s)) do
        (format t "> new message~%")
        (format t "  transaction id: ~d~%" t-id)
        (format t "  protocol id:    ~d~%" p-id)
        (format t "  size:           ~d~%" size)
        (format t "  unit id:        ~d~%" u-id)
        (format t "  function code:  ~2,'0x~%" f-code)
        (format t "  rest:           ~{~2,'0x~^ ~}~%" (loop repeat size collect (read-byte s)))
        (finish-output)
        (write-mbap s t-id p-id 3 u-id (+ #x80 f-code))
        (write-byte #x04 s)
        (finish-output s))
      (sb-sys:io-timeout ()
        (format t "> client timed out~%"))
      (end-of-file ()
        (format t "> stream closed by client~%"))))
  (sb-bsd-sockets:socket-close c))

(defun make-server (&key (port 502) (remote-access nil))
  "Starts a simple modbus server.
   port defaults to 502
   remote-access defaults to NIL, if T it binds to all interfaces"
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket (if remote-access '(0 0 0 0) '(127 0 0 1)) port)
    (sb-bsd-sockets:socket-listen socket 5)
    (format t "> server started on ~a~%" port)
    (handler-case
      (loop for c = (sb-bsd-sockets:socket-accept socket) do
        (format t "> connection accepted~%")
        (sb-thread:make-thread (lambda () (handle-client c))))
      (sb-sys:interactive-interrupt ()
        (format t "> stopping server")))
    (sb-bsd-sockets:socket-close socket)))

(defun read-registers (host address &key (quantity 1) (unit-id #xFF) (port 502))
  "Reads holding registers.
   host should be in format '(127 0 0 1)
   address is the starting address
   quantity defaults to 1
   unit-id defaults to #xFF
   port defaults to 502"
  (let ((vals nil) (err nil) (socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect socket host port)
    (let ((s (sb-bsd-sockets:socket-make-stream socket :input t :output t :element-type :default :timeout 3)))
      (write-mbap s nil 0 4 unit-id #x03)
      (write-int 2 address s)
      (write-int 2 quantity s)
      (finish-output s)
      (multiple-value-bind (t-id p-id size u-id f-code) (read-mbap s)
        (declare (ignore size) (ignore u-id))
        (cond
          ((not (= t-id *t-id*)) (setf err (format nil "bad transaction id: ~d != ~d" t-id *t-id*)))
          ((not (= p-id 0)) (setf err (format nil "bad protocol id: ~d != 0" p-id)))
          ((> f-code #x80) (setf err (format nil "error code: #x~2,'0x" (read-byte s))))
          (t (setf vals (loop repeat (/ (read-byte s) 2) collect (read-int 2 s)))))))
    (sb-bsd-sockets:socket-close socket)
    (values vals err)))

(defun read-coils (host address &key (quantity 1) (unit-id #xFF) (port 502))
  "Reads coils.
   host should be in format '(127 0 0 1)
   address is the starting address
   quantity defaults to 1
   unit-id defaults to #xFF
   port defaults to 502"
  (let ((bits nil) (err nil) (socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect socket host port)
    (let ((s (sb-bsd-sockets:socket-make-stream socket :input t :output t :element-type :default :timeout 3)))
      (write-mbap s nil 0 4 unit-id #x01)
      (write-int 2 address s)
      (write-int 2 quantity s)
      (finish-output s)
      (multiple-value-bind (t-id p-id size u-id f-code) (read-mbap s)
        (declare (ignore size) (ignore u-id))
        (cond
          ((not (= t-id *t-id*)) (setf err (format nil "bad transaction id: ~d != ~d" t-id *t-id*)))
          ((not (= p-id 0)) (setf err (format nil "bad protocol id: ~d != 0" p-id)))
          ((> f-code #x80) (setf err (format nil "error code: #x~2,'0x" (read-byte s))))
          (t (setf bits (loop repeat (read-byte s) collect (int->bits 8 (read-byte s))))))))
    (sb-bsd-sockets:socket-close socket)
    (values (subseq (apply #'append bits) 0 quantity) err)))
