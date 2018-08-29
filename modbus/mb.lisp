;;; A Simple MODBUS TCP Server and Client Implementation for Clozure CL


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

(defun make-server (&key (host "localhost") (port 6502))
  "Starts a simple modbus server."
  (with-open-socket (socket :type :stream
                            :connect :passive
                            :local-host host
                            :local-port port
                            :reuse-address t
                            :input-timeout 1
                            :output-timeout 1
                            :connect-timeout 2)
    (format t "> server started ~a:~a~%" host port)
    (loop for s = (accept-connection socket :wait t) do
      (format t "> connection accepted~%")
      (format t "  transaction id: ~d~%" (read-int 2 s))
      (format t "  protocol id:    ~d~%" (read-int 2 s))
      (format t "  size:           ~d~%" (read-int 2 s))
      (format t "  unit id:        ~d~%" (read-byte s))
      (format t "  function code:  ~d~%" (read-byte s))
      (format t "  address:        ~d~%" (read-int 2 s))
      (format t "  quantity:       ~d~%" (read-int 2 s))
      (finish-output))))


(defun read-register (host address &key (quantity 1) (unit 0) (port 6502))
  "Reads holding registers."
  (with-open-socket (s :type :stream
                       :connect :active
                       :remote-host host
                       :remote-port port
                       :input-timeout 1
                       :output-timeout 1
                       :connect-timeout 2)
    (format t "> connection established~%")
    (write-int 2 4242 s)        ; transaction id
    (write-int 2 0 s)           ; protocol id
    (write-int 2 6 s)           ; message size
    (write-byte unit s)         ; unit id
    (write-byte 3 s)            ; function code (read holding registers
    (write-int 2 address s)     ; starting address
    (write-int 2 quantity s)))  ; quantity of registers
