;;; A Simple MODBUS TCP Server and Client Implementation for Clozure CL

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
    (read-int 2 stream)       ; protocol id
    (- (read-int 2 stream) 2) ; size (adjusted because we also read the unit id and function code)
    (read-byte stream)        ; unit id
    (read-byte stream)))      ; function code

(defun write-mbap (stream size unit-id function-code)
  (setf *t-id* (mod (1+ *t-id*) #xFFFF))
  (write-int 2 *t-id* stream)
  (write-int 2 0 stream)
  (write-int 2 (+ 2 size) stream)
  (write-byte unit-id stream)
  (write-byte function-code stream))

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
        (write-int 2 t-id s)
        (write-int 2 p-id s)
        (write-int 2 3 s)  ; response size
        (write-byte u-id s)
        ; send error response (4, server error)
        (write-byte (+ #x80 f-code) s)
        (write-byte #x04 s)
        ; send success response with dummy data
        ; (write-byte f-code s)
        ; (write-byte 2 s)  ; count of register value bytes
        ; (write-int 2 #xFFFF s)
        (finish-output s))
      (sb-sys:io-timeout ()
        (format t "> client timed out~%"))
      (end-of-file ()
        (format t "> stream closed by client~%"))))
  (sb-bsd-sockets:socket-close c))

(defun make-server (&key (port 6502) (remote-access nil))
  "Starts a simple modbus server."
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

(defun read-registers (host address &key (quantity 1) (unit-id #xFF) (port 6502))
  "Reads holding registers."
  (let ((vals nil) (err nil) (socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect socket host port)
    (let ((s (sb-bsd-sockets:socket-make-stream socket :input t :output t :element-type :default :timeout 3)))
      (write-mbap s 4 unit-id #x03)
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

(defun read-coils (host address &key (quantity 1) (unit-id #xFF) (port 6502))
  "Reads coils."
  (let ((bits nil) (err nil) (socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect socket host port)
    (let ((s (sb-bsd-sockets:socket-make-stream socket :input t :output t :element-type :default :timeout 3)))
      (write-mbap s 4 unit-id #x01)
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
