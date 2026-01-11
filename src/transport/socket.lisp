;;;; socket.lisp - TCP socket wrapper using usocket
;;;;
;;;; Provides portable TCP socket operations for gRPC communication

(in-package #:clgrpc.transport)

;;; Socket wrapper structure

(defstruct tcp-socket
  "Wrapper for usocket with associated stream."
  usocket          ; The underlying usocket
  stream           ; The socket stream for I/O
  host             ; Remote host
  port             ; Remote port
  timeout)         ; I/O timeout in seconds

;;; Socket options

(defun set-tcp-nodelay (socket)
  "Set TCP_NODELAY on socket to disable Nagle's algorithm.
   This prevents 40ms delays from delayed ACK + Nagle interaction."
  #+sbcl
  (handler-case
      (let ((usock (tcp-socket-usocket socket)))
        (setf (sb-bsd-sockets:sockopt-tcp-nodelay (usocket:socket usock)) t))
    (error (e)
      (warn "Failed to set TCP_NODELAY: ~A" e)))
  #-sbcl
  (warn "TCP_NODELAY not implemented for this Lisp"))

;;; Socket creation and connection

(defun make-tcp-connection (host port &key (timeout 30))
  "Create a TCP connection to host:port.

   Args:
     host: String hostname or IP address
     port: Integer port number
     timeout: Connection timeout in seconds (default 30)

   Returns:
     tcp-socket structure"
  (handler-case
      (let* ((usock (usocket:socket-connect host port
                                           :element-type '(unsigned-byte 8)
                                           :timeout timeout))
             (stream (usocket:socket-stream usock)))
        (let ((tcp-sock (make-tcp-socket
                         :usocket usock
                         :stream stream
                         :host host
                         :port port
                         :timeout timeout)))
          ;; Set TCP_NODELAY to disable Nagle's algorithm
          (set-tcp-nodelay tcp-sock)
          tcp-sock))
    (error (e)
      (error "Failed to connect to ~A:~D: ~A" host port e))))

(defun make-tcp-server (port &key (host "0.0.0.0") (reuse-address t))
  "Create a TCP server socket listening on host:port.

   Args:
     port: Integer port number to listen on
     host: String address to bind to (default 0.0.0.0)
     reuse-address: Boolean to set SO_REUSEADDR (default t)

   Returns:
     usocket server socket"
  (handler-case
      (usocket:socket-listen host port
                            :element-type '(unsigned-byte 8)
                            :reuse-address reuse-address
                            :backlog 128)
    (error (e)
      (error "Failed to create server on ~A:~D: ~A" host port e))))

(defun accept-connection (server-socket &key (timeout nil))
  "Accept a new connection from a server socket.

   Args:
     server-socket: usocket server socket
     timeout: Accept timeout in seconds (nil for blocking)

   Returns:
     tcp-socket structure for the new connection"
  (handler-case
      (let* ((client-sock (if timeout
                             (usocket:socket-accept server-socket
                                                   :element-type '(unsigned-byte 8))
                             (usocket:socket-accept server-socket
                                                   :element-type '(unsigned-byte 8))))
             (stream (usocket:socket-stream client-sock))
             (peer-host (usocket:get-peer-address client-sock))
             (peer-port (usocket:get-peer-port client-sock)))
        (let ((tcp-sock (make-tcp-socket
                         :usocket client-sock
                         :stream stream
                         :host (usocket:vector-quad-to-dotted-quad peer-host)
                         :port peer-port
                         :timeout nil)))
          ;; Set TCP_NODELAY to disable Nagle's algorithm
          (set-tcp-nodelay tcp-sock)
          tcp-sock))
    (error (e)
      (error "Failed to accept connection: ~A" e))))

;;; Socket I/O operations

(defun socket-read-byte (socket)
  "Read a single byte from socket.

   Returns:
     Unsigned byte (0-255) or nil on EOF"
  (handler-case
      (read-byte (tcp-socket-stream socket) nil nil)
    (error (e)
      (error "Socket read error: ~A" e))))

(defun socket-read-bytes (socket count)
  "Read exactly count bytes from socket.

   Args:
     socket: tcp-socket structure
     count: Number of bytes to read

   Returns:
     Byte array of length count

   Signals error if EOF before count bytes read."
  (let ((buffer (make-array count :element-type '(unsigned-byte 8)))
        (stream (tcp-socket-stream socket)))
    (handler-case
        (let ((bytes-read (read-sequence buffer stream)))
          (unless (= bytes-read count)
            (error "Unexpected EOF: read ~D bytes, expected ~D" bytes-read count))
          buffer)
      (error (e)
        (error "Socket read error: ~A" e)))))

(defun socket-read-sequence (socket buffer &key (start 0) (end nil))
  "Read bytes into buffer from socket.

   Args:
     socket: tcp-socket structure
     buffer: Byte array to read into
     start: Starting index in buffer (default 0)
     end: Ending index in buffer (default nil = length)

   Returns:
     Number of bytes actually read"
  (let ((stream (tcp-socket-stream socket))
        (max-bytes (- (or end (length buffer)) start)))
    (handler-case
        ;; Read at least one byte (blocking), then read more while available (non-blocking)
        ;; This prevents: 1) blocking on full buffer, 2) returning 0 bytes when data will arrive
        (let ((bytes-read 0))
          ;; Read first byte (blocking) - returns nil on EOF
          (let ((first-byte (read-byte stream nil nil)))
            (if first-byte
                (progn
                  (setf (aref buffer start) first-byte)
                  (setf bytes-read 1)
                  ;; Read more bytes while data available (non-blocking)
                  (loop while (and (< bytes-read max-bytes)
                                  (listen stream))
                        do (let ((byte (read-byte stream nil nil)))
                             (if byte
                                 (progn
                                   (setf (aref buffer (+ start bytes-read)) byte)
                                   (incf bytes-read))
                                 (return)))))  ; EOF during non-blocking read
                (setf bytes-read 0)))  ; EOF on first byte
          bytes-read)
      (error (e)
        (error "Socket read error: ~A" e)))))

(defun socket-write-byte (socket byte)
  "Write a single byte to socket.

   Args:
     socket: tcp-socket structure
     byte: Unsigned byte (0-255) to write"
  (handler-case
      (write-byte byte (tcp-socket-stream socket))
    (error (e)
      (error "Socket write error: ~A" e))))

(defun socket-write-bytes (socket bytes)
  "Write byte array to socket.

   Args:
     socket: tcp-socket structure
     bytes: Byte array or sequence to write

   Returns:
     bytes (the input)"
  (handler-case
      (progn
        (write-sequence bytes (tcp-socket-stream socket))
        bytes)
    (error (e)
      (error "Socket write error: ~A" e))))

(defun socket-write-sequence (socket sequence &key (start 0) (end nil))
  "Write a portion of sequence to socket.

   Args:
     socket: tcp-socket structure
     sequence: Byte array or sequence
     start: Starting index (default 0)
     end: Ending index (default nil = length)

   Returns:
     sequence (the input)"
  (handler-case
      (progn
        (write-sequence sequence (tcp-socket-stream socket)
                       :start start
                       :end (or end (length sequence)))
        sequence)
    (error (e)
      (error "Socket write error: ~A" e))))

(defun socket-flush (socket)
  "Flush any buffered output on socket.

   Args:
     socket: tcp-socket structure"
  (handler-case
      (force-output (tcp-socket-stream socket))
    (error (e)
      (error "Socket flush error: ~A" e))))

;;; Socket control and status

(defun socket-close (socket)
  "Close a socket connection.

   Args:
     socket: tcp-socket structure"
  (handler-case
      (when (tcp-socket-usocket socket)
        (usocket:socket-close (tcp-socket-usocket socket))
        (setf (tcp-socket-usocket socket) nil
              (tcp-socket-stream socket) nil))
    (error (e)
      (warn "Error closing socket: ~A" e))))

(defun socket-open-p (socket)
  "Check if socket is still open.

   Args:
     socket: tcp-socket structure

   Returns:
     Boolean indicating if socket is open"
  (and (tcp-socket-usocket socket)
       (usocket:socket-stream (tcp-socket-usocket socket))
       (open-stream-p (tcp-socket-stream socket))))

(defun socket-wait-for-input (socket &key (timeout nil))
  "Wait for data to be available on socket.

   Args:
     socket: tcp-socket structure
     timeout: Timeout in seconds (nil for blocking)

   Returns:
     Boolean indicating if data is available"
  (handler-case
      (let ((ready (usocket:wait-for-input (tcp-socket-usocket socket)
                                           :timeout timeout
                                           :ready-only t)))
        (not (null ready)))
    (error (e)
      (warn "Error waiting for input: ~A" e)
      nil)))

(defun socket-get-peer-info (socket)
  "Get remote peer information.

   Args:
     socket: tcp-socket structure

   Returns:
     (values host port)"
  (values (tcp-socket-host socket)
          (tcp-socket-port socket)))

(defun socket-set-timeout (socket timeout)
  "Set I/O timeout for socket operations.

   Args:
     socket: tcp-socket structure
     timeout: Timeout in seconds"
  (setf (tcp-socket-timeout socket) timeout))

;;; High-level utilities

(defmacro with-tcp-connection ((socket-var host port &key (timeout 30)) &body body)
  "Execute body with an open TCP connection, ensuring cleanup.

   Usage:
     (with-tcp-connection (sock \"localhost\" 8080)
       (socket-write-bytes sock #(1 2 3))
       (socket-read-bytes sock 10))"
  `(let ((,socket-var nil))
     (unwind-protect
          (progn
            (setf ,socket-var (make-tcp-connection ,host ,port :timeout ,timeout))
            ,@body)
       (when ,socket-var
         (socket-close ,socket-var)))))

(defmacro with-tcp-server ((server-var port &key (host "0.0.0.0")) &body body)
  "Execute body with an open TCP server socket, ensuring cleanup.

   Usage:
     (with-tcp-server (server 8080)
       (loop for client = (accept-connection server)
             do (handle-client client)))"
  `(let ((,server-var nil))
     (unwind-protect
          (progn
            (setf ,server-var (make-tcp-server ,port :host ,host))
            ,@body)
       (when ,server-var
         (usocket:socket-close ,server-var)))))
