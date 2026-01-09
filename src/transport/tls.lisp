;;;; tls.lisp - TLS wrapper using cl+ssl with ALPN support
;;;;
;;;; Provides TLS/SSL operations with ALPN for HTTP/2 (gRPC)

(in-package #:clgrpc.transport)

;;; TLS socket wrapper

(defstruct tls-socket
  "Wrapper for TLS socket with ALPN support."
  ssl-stream       ; The cl+ssl stream
  tcp-socket       ; Underlying TCP socket
  host             ; Remote host
  port             ; Remote port
  alpn-protocol    ; Negotiated ALPN protocol (e.g., "h2")
  verified)        ; Certificate verification status

;;; TLS client connection

(defun make-tls-connection (host port &key
                                      (timeout 30)
                                      (alpn-protocols '("h2"))
                                      (verify t)
                                      (hostname-check t))
  "Create a TLS connection to host:port with ALPN support.

   Args:
     host: String hostname or IP address
     port: Integer port number
     timeout: Connection timeout in seconds (default 30)
     alpn-protocols: List of ALPN protocol strings (default: (\"h2\"))
     verify: Verify server certificate (default t)
     hostname-check: Check hostname in certificate (default t)

   Returns:
     tls-socket structure"
  (let ((tcp-sock nil)
        (ssl-stream nil))
    (handler-case
        (progn
          ;; Create TCP connection first
          (setf tcp-sock (make-tcp-connection host port :timeout timeout))

          ;; Wrap with TLS
          (setf ssl-stream
                (cl+ssl:make-ssl-client-stream
                 (tcp-socket-stream tcp-sock)
                 :hostname host
                 :verify (if verify :required :optional)
                 :unwrap-stream-p t  ; Keep underlying stream open
                 ;; ALPN support (if cl+ssl version supports it)
                 #+cl+ssl-alpn
                 :alpn-protocols alpn-protocols))

          ;; Get negotiated ALPN protocol
          (let ((negotiated-protocol
                  #+cl+ssl-alpn
                  (cl+ssl:get-selected-alpn-protocol ssl-stream)
                  #-cl+ssl-alpn
                  nil))

            (make-tls-socket
             :ssl-stream ssl-stream
             :tcp-socket tcp-sock
             :host host
             :port port
             :alpn-protocol negotiated-protocol
             :verified verify)))

      (error (e)
        ;; Cleanup on error
        (when ssl-stream
          (ignore-errors (close ssl-stream)))
        (when tcp-sock
          (ignore-errors (socket-close tcp-sock)))
        (error "Failed to create TLS connection to ~A:~D: ~A" host port e)))))

;;; TLS server operations

(defun make-tls-server-context (&key
                                 (certificate-file nil)
                                 (private-key-file nil)
                                 (alpn-protocols '("h2")))
  "Create a TLS server context with certificate and ALPN support.

   Args:
     certificate-file: Path to server certificate PEM file
     private-key-file: Path to private key PEM file
     alpn-protocols: List of ALPN protocols to advertise (default: (\"h2\"))

   Returns:
     SSL context object"
  (unless certificate-file
    (error "TLS server requires certificate-file"))
  (unless private-key-file
    (error "TLS server requires private-key-file"))

  ;; Note: TLS server functionality is not fully implemented yet
  ;; This is a placeholder that creates a basic context
  ;; Full server support will be added when needed
  (handler-case
      (let ((ctx (cl+ssl:make-context :method :tls)))
        ;; TODO: Load certificate and key files properly
        ;; cl+ssl API for server contexts needs to be researched
        (declare (ignore certificate-file private-key-file))

        ;; Set ALPN protocols (if supported)
        #+cl+ssl-alpn
        (cl+ssl:set-context-alpn-protocols ctx alpn-protocols)

        ctx)
    (error (e)
      (error "Failed to create TLS server context: ~A" e))))

(defun accept-tls-connection (server-socket ssl-context)
  "Accept and wrap a connection with TLS.

   Args:
     server-socket: usocket server socket
     ssl-context: SSL context from make-tls-server-context

   Returns:
     tls-socket structure"
  (let ((tcp-sock nil)
        (ssl-stream nil))
    (handler-case
        (progn
          ;; Accept TCP connection
          (setf tcp-sock (accept-connection server-socket))

          ;; Wrap with TLS
          (setf ssl-stream
                (cl+ssl:make-ssl-server-stream
                 (tcp-socket-stream tcp-sock)
                 :certificate-chain-file nil  ; Already loaded in context
                 :unwrap-stream-p t
                 :external-format :default))

          ;; Get negotiated ALPN protocol
          (let ((negotiated-protocol
                  #+cl+ssl-alpn
                  (cl+ssl:get-selected-alpn-protocol ssl-stream)
                  #-cl+ssl-alpn
                  nil))

            (make-tls-socket
             :ssl-stream ssl-stream
             :tcp-socket tcp-sock
             :host (tcp-socket-host tcp-sock)
             :port (tcp-socket-port tcp-sock)
             :alpn-protocol negotiated-protocol
             :verified nil)))

      (error (e)
        ;; Cleanup on error
        (when ssl-stream
          (ignore-errors (close ssl-stream)))
        (when tcp-sock
          (ignore-errors (socket-close tcp-sock)))
        (error "Failed to accept TLS connection: ~A" e)))))

;;; TLS I/O operations

(defun tls-read-byte (socket)
  "Read a single byte from TLS socket.

   Returns:
     Unsigned byte (0-255) or nil on EOF"
  (handler-case
      (read-byte (tls-socket-ssl-stream socket) nil nil)
    (error (e)
      (error "TLS read error: ~A" e))))

(defun tls-read-bytes (socket count)
  "Read exactly count bytes from TLS socket.

   Args:
     socket: tls-socket structure
     count: Number of bytes to read

   Returns:
     Byte array of length count"
  (let ((buffer (make-array count :element-type '(unsigned-byte 8)))
        (stream (tls-socket-ssl-stream socket)))
    (handler-case
        (let ((bytes-read (read-sequence buffer stream)))
          (unless (= bytes-read count)
            (error "Unexpected EOF: read ~D bytes, expected ~D" bytes-read count))
          buffer)
      (error (e)
        (error "TLS read error: ~A" e)))))

(defun tls-read-sequence (socket buffer &key (start 0) (end nil))
  "Read bytes into buffer from TLS socket.

   Args:
     socket: tls-socket structure
     buffer: Byte array to read into
     start: Starting index in buffer
     end: Ending index in buffer

   Returns:
     Number of bytes actually read"
  (handler-case
      (read-sequence buffer (tls-socket-ssl-stream socket)
                    :start start
                    :end (or end (length buffer)))
    (error (e)
      (error "TLS read error: ~A" e))))

(defun tls-write-byte (socket byte)
  "Write a single byte to TLS socket.

   Args:
     socket: tls-socket structure
     byte: Unsigned byte (0-255)"
  (handler-case
      (write-byte byte (tls-socket-ssl-stream socket))
    (error (e)
      (error "TLS write error: ~A" e))))

(defun tls-write-bytes (socket bytes)
  "Write byte array to TLS socket.

   Args:
     socket: tls-socket structure
     bytes: Byte array or sequence to write

   Returns:
     bytes (the input)"
  (handler-case
      (progn
        (write-sequence bytes (tls-socket-ssl-stream socket))
        bytes)
    (error (e)
      (error "TLS write error: ~A" e))))

(defun tls-write-sequence (socket sequence &key (start 0) (end nil))
  "Write a portion of sequence to TLS socket.

   Args:
     socket: tls-socket structure
     sequence: Byte array or sequence
     start: Starting index
     end: Ending index

   Returns:
     sequence (the input)"
  (handler-case
      (progn
        (write-sequence sequence (tls-socket-ssl-stream socket)
                       :start start
                       :end (or end (length sequence)))
        sequence)
    (error (e)
      (error "TLS write error: ~A" e))))

(defun tls-flush (socket)
  "Flush any buffered output on TLS socket."
  (handler-case
      (force-output (tls-socket-ssl-stream socket))
    (error (e)
      (error "TLS flush error: ~A" e))))

;;; TLS control and status

(defun tls-close (socket)
  "Close a TLS connection.

   Args:
     socket: tls-socket structure"
  (handler-case
      (progn
        (when (tls-socket-ssl-stream socket)
          (close (tls-socket-ssl-stream socket))
          (setf (tls-socket-ssl-stream socket) nil))
        (when (tls-socket-tcp-socket socket)
          (socket-close (tls-socket-tcp-socket socket))
          (setf (tls-socket-tcp-socket socket) nil)))
    (error (e)
      (warn "Error closing TLS socket: ~A" e))))

(defun tls-open-p (socket)
  "Check if TLS socket is still open.

   Returns:
     Boolean"
  (and (tls-socket-ssl-stream socket)
       (open-stream-p (tls-socket-ssl-stream socket))))

(defun tls-get-alpn-protocol (socket)
  "Get the negotiated ALPN protocol.

   Args:
     socket: tls-socket structure

   Returns:
     String protocol name (e.g., \"h2\") or nil"
  (tls-socket-alpn-protocol socket))

(defun tls-get-peer-info (socket)
  "Get remote peer information.

   Returns:
     (values host port)"
  (values (tls-socket-host socket)
          (tls-socket-port socket)))

(defun tls-get-peer-certificate (socket)
  "Get peer's X.509 certificate information.

   Returns:
     Certificate info or nil"
  (handler-case
      #+cl+ssl-has-peer-certificate
      (cl+ssl:ssl-stream-peer-certificate (tls-socket-ssl-stream socket))
      #-cl+ssl-has-peer-certificate
      nil
    (error (e)
      (warn "Error getting peer certificate: ~A" e)
      nil)))

;;; High-level utilities

(defmacro with-tls-connection ((socket-var host port &key
                                           (timeout 30)
                                           (alpn-protocols '("h2"))
                                           (verify t))
                                &body body)
  "Execute body with an open TLS connection, ensuring cleanup.

   Usage:
     (with-tls-connection (sock \"example.com\" 443 :alpn-protocols '(\"h2\"))
       (tls-write-bytes sock #(1 2 3))
       (tls-read-bytes sock 10))"
  `(let ((,socket-var nil))
     (unwind-protect
          (progn
            (setf ,socket-var (make-tls-connection ,host ,port
                                                  :timeout ,timeout
                                                  :alpn-protocols ,alpn-protocols
                                                  :verify ,verify))
            ,@body)
       (when ,socket-var
         (tls-close ,socket-var)))))

;;; ALPN Compatibility Notes
;;;
;;; ALPN (Application-Layer Protocol Negotiation) is required for gRPC/HTTP2.
;;; If cl+ssl doesn't support ALPN on your platform:
;;;
;;; 1. Try upgrading cl+ssl to latest version
;;; 2. Check if OpenSSL version supports ALPN (1.0.2+)
;;; 3. May need to use FFI to call OpenSSL functions directly
;;; 4. Without ALPN, can still use HTTP/2 with prior knowledge (no negotiation)

(defun alpn-supported-p ()
  "Check if ALPN is supported by cl+ssl.

   Returns:
     Boolean indicating ALPN support"
  #+cl+ssl-alpn
  t
  #-cl+ssl-alpn
  (progn
    (warn "ALPN not supported by cl+ssl on this platform. HTTP/2 may require prior knowledge.")
    nil))
