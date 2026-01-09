;;;; connection.lisp - HTTP/2 connection management

(in-package #:clgrpc.http2)

;;; HTTP/2 Connection Preface (RFC 9113 Section 3.4)

(defparameter +http2-client-preface+
  (format nil "PRI * HTTP/2.0~C~C~C~CSM~C~C~C~C"
          #\Return #\Linefeed #\Return #\Linefeed
          #\Return #\Linefeed #\Return #\Linefeed)
  "HTTP/2 client connection preface")

(defun http2-client-preface-bytes ()
  "Get HTTP/2 client preface as byte array"
  (babel:string-to-octets +http2-client-preface+ :encoding :latin-1))

;;; HTTP/2 Connection

(defstruct http2-connection
  "HTTP/2 connection state"
  (socket nil)  ; Underlying socket/stream
  (is-client t :type boolean)  ; Client or server
  (streams (make-hash-table :test 'eql) :type hash-table)  ; Stream ID -> http2-stream
  (active-calls (make-hash-table :test 'eql) :type hash-table)  ; Stream ID -> grpc-call (for client)
  (next-stream-id 1 :type (unsigned-byte 31))  ; Next stream ID to use
  (local-settings (make-default-settings))
  (remote-settings (make-default-settings))
  (hpack-encoder (make-hpack-context))
  (hpack-decoder (make-hpack-context))
  (connection-send-window (make-flow-control-window))
  (connection-recv-window (make-flow-control-window))
  (write-lock (bt:make-lock "connection-write-lock"))
  (last-stream-id 0 :type (unsigned-byte 31))  ; Last stream ID received
  (goaway-sent nil :type boolean)
  (goaway-received nil :type boolean)
  (closed nil :type boolean)
  (settings-received nil :type boolean)  ; Server SETTINGS received (connection ready)
  (ready-lock (bt:make-lock "connection-ready-lock"))
  (ready-cv (bt:make-condition-variable :name "connection-ready")))

(defun make-client-connection (socket)
  "Create HTTP/2 client connection"
  (make-http2-connection :socket socket
                        :is-client t
                        :next-stream-id 1))  ; Clients use odd stream IDs

(defun make-server-connection (socket)
  "Create HTTP/2 server connection"
  (make-http2-connection :socket socket
                        :is-client nil
                        :next-stream-id 2))  ; Servers use even stream IDs

;;; Stream Management

(defun connection-get-stream (connection stream-id)
  "Get stream by ID, or nil if doesn't exist"
  (gethash stream-id (http2-connection-streams connection)))

(defun connection-create-stream (connection stream-id)
  "Create new stream with given ID"
  (let ((stream (make-http2-stream :id stream-id)))
    (setf (gethash stream-id (http2-connection-streams connection)) stream)
    stream))

(defun connection-get-or-create-stream (connection stream-id)
  "Get existing stream or create new one"
  (or (connection-get-stream connection stream-id)
      (connection-create-stream connection stream-id)))

(defun connection-allocate-stream-id (connection)
  "Allocate next stream ID for client/server"
  (let ((stream-id (http2-connection-next-stream-id connection)))
    (incf (http2-connection-next-stream-id connection) 2)  ; Skip by 2 (odd/even)
    stream-id))

;;; Frame Sending

(defun connection-send-frame (connection frame)
  "Send frame on connection (thread-safe)"
  (bt:with-lock-held ((http2-connection-write-lock connection))
    (write-frame-to-stream frame (http2-connection-socket connection))))

(defun connection-send-settings (connection &optional (settings nil))
  "Send SETTINGS frame"
  (let ((frame (if settings
                  (make-settings-frame-from-settings settings)
                  (make-settings-frame))))
    (connection-send-frame connection frame)))

(defun connection-send-settings-ack (connection)
  "Send SETTINGS ACK frame"
  (let ((frame (make-settings-frame :ack t)))
    (connection-send-frame connection frame)))

(defun connection-send-window-update (connection stream-id increment)
  "Send WINDOW_UPDATE frame"
  (let ((frame (make-window-update-frame stream-id increment)))
    (connection-send-frame connection frame)))

(defun connection-send-ping (connection opaque-data &key ack)
  "Send PING frame"
  (let ((frame (make-ping-frame opaque-data :ack ack)))
    (connection-send-frame connection frame)))

(defun connection-send-goaway (connection last-stream-id error-code &optional debug-data)
  "Send GOAWAY frame and mark connection as closing"
  (setf (http2-connection-goaway-sent connection) t)
  (let ((frame (make-goaway-frame last-stream-id error-code debug-data)))
    (connection-send-frame connection frame)))

;;; Frame Receiving

(defun connection-read-frame (connection)
  "Read next frame from connection. Returns frame or nil on EOF."
  (read-frame-from-stream (http2-connection-socket connection)))

;;; Connection Preface Handling

(defun connection-send-client-preface (connection)
  "Send HTTP/2 client connection preface + SETTINGS"
  (let ((socket (http2-connection-socket connection)))
    ;; Send preface string
    (write-sequence (http2-client-preface-bytes) socket)
    (force-output socket)
    ;; Send initial SETTINGS
    (connection-send-settings connection (http2-connection-local-settings connection))))

(defun connection-receive-client-preface (connection)
  "Receive and verify HTTP/2 client connection preface (server side)"
  (let* ((expected (http2-client-preface-bytes))
         (received (read-bytes-from-stream (http2-connection-socket connection)
                                          (length expected))))
    (unless (equalp received expected)
      (signal-protocol-error "Invalid HTTP/2 client preface"))
    ;; Send server SETTINGS
    (connection-send-settings connection (http2-connection-local-settings connection))))

;;; Connection Initialization

(defun initialize-client-connection (connection)
  "Initialize HTTP/2 client connection (send preface)"
  (connection-send-client-preface connection))

(defun initialize-server-connection (connection)
  "Initialize HTTP/2 server connection (receive preface, send SETTINGS)"
  (connection-receive-client-preface connection))

;;; Basic Frame Processing (stub for now)

(defun process-settings-frame (connection frame)
  "Process received SETTINGS frame"
  (if (frame-flag-set-p frame +flag-ack+)
      ;; ACK - settings have been applied
      nil
      ;; New settings
      (let ((new-settings (parse-settings-frame frame)))
        (when new-settings
          (setf (http2-connection-remote-settings connection)
                (apply-settings (http2-connection-remote-settings connection)
                               new-settings))
          ;; Send ACK
          (connection-send-settings-ack connection)))))

(defun process-ping-frame (connection frame)
  "Process received PING frame"
  (unless (frame-flag-set-p frame +flag-ack+)
    ;; Send PING ACK with same opaque data
    (connection-send-ping connection (frame-payload frame) :ack t)))

(defun process-window-update-frame (connection frame)
  "Process received WINDOW_UPDATE frame"
  (let ((stream-id (frame-stream-id frame)))
    (if (zerop stream-id)
        ;; Connection-level window update
        (process-window-update (http2-connection-connection-send-window connection)
                             frame)
        ;; Stream-level window update
        (let ((stream (connection-get-stream connection stream-id)))
          (when stream
            (process-window-update (http2-stream-send-window stream) frame))))))

(defun process-goaway-frame (connection frame)
  "Process received GOAWAY frame"
  (setf (http2-connection-goaway-received connection) t)
  (let ((payload (frame-payload frame)))
    (let ((last-stream-id (logand (decode-uint32-be payload 0) #x7FFFFFFF))
          (error-code (decode-uint32-be payload 4)))
      (setf (http2-connection-last-stream-id connection) last-stream-id)
      ;; Could process error-code and debug data here
      (values last-stream-id error-code))))

;;; Connection State

(defun connection-is-closed-p (connection)
  "Check if connection is closed"
  (or (http2-connection-closed connection)
      (and (http2-connection-goaway-sent connection)
           (http2-connection-goaway-received connection))))

(defun connection-close (connection)
  "Close connection"
  (setf (http2-connection-closed connection) t)
  (when (http2-connection-socket connection)
    (clgrpc.transport:buffered-close (http2-connection-socket connection))))
