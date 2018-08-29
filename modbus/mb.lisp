;;; A Simple MODBUS TCP Server and Client Implementation for Clozure CL

(defparameter *t-id* 0)

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

(defun handle-client (s)
  "Handle a modbus client connection stream."
  (handler-case
    (loop for (t-id p-id size u-id f-code) = (multiple-value-list (read-mbap s)) do
      (format t "> new message~%")
      (format t "  transaction id: ~d~%" t-id)
      (format t "  protocol id:    ~d~%" p-id)
      (format t "  size:           ~d~%" size)
      (format t "  unit id:        ~d~%" u-id)
      (format t "  function code:  ~d~%" f-code)
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
    (socket-error (c)
      (format t "> socket error: ~a~%" c))
    (end-of-file ()
      (format t "> stream closed by client~%"))
    ((or input-timeout output-timeout) ()
      (format t "> closing due to inactivity~%")))
  (close s))

(defun make-server (&key (port 6502) (remote-access nil))
  "Starts a simple modbus server."
  (with-open-socket (socket :type :stream
                            :connect :passive
                            :local-host (if remote-access "0.0.0.0" "localhost")
                            :local-port port
                            :reuse-address t
                            :input-timeout 30
                            :output-timeout 30
                            :connect-timeout 3
                            :keepalive t)
    (format t "> server started on ~a~%" port)
    (loop for s = (accept-connection socket :wait t) do
      (format t "> connection accepted~%")
      (process-run-function "mb-client" (lambda () (handle-client s))))))

(defun read-registers (host address &key (quantity 1) (unit #xFF) (port 6502))
  "Reads holding registers."
  (with-open-socket (s :type :stream
                       :connect :active
                       :remote-host host
                       :remote-port port
                       :input-timeout 1
                       :output-timeout 1
                       :connect-timeout 2)
    (format t "> connection established~%")
    (setf *t-id* (mod (1+ *t-id*) #xFFFF))
    (write-int 2 *t-id* s)      ; transaction id
    (write-int 2 0 s)           ; protocol id
    (write-int 2 6 s)           ; message size
    (write-byte unit s)         ; unit id
    (write-byte 3 s)            ; function code (3, read holding registers)
    (write-int 2 address s)     ; starting address
    (write-int 2 quantity s)))  ; quantity of registers
