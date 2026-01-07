;;;; protocol.lisp - gRPC protocol implementation (gRPC HTTP/2 spec)
;;;
;;; gRPC uses HTTP/2 as transport with specific message framing.
;;; Each message is length-prefixed:
;;;   Compressed-Flag (1 byte): 0 = no compression, 1 = compressed
;;;   Message-Length  (4 bytes): Big-endian uint32 length
;;;   Message         (N bytes): Protobuf message payload

(in-package #:clgrpc.grpc)

;;; gRPC Message Framing Constants

(defconstant +grpc-compressed-flag-none+ 0
  "No compression")

(defconstant +grpc-compressed-flag-gzip+ 1
  "GZIP compression (future support)")

(defconstant +grpc-message-header-size+ 5
  "Size of gRPC message header (1 byte compressed + 4 bytes length)")

(defconstant +grpc-max-message-size+ #x7FFFFFFF
  "Maximum gRPC message size (2GB - 1)")

;;; gRPC Message Encoding

(defun encode-grpc-message (message-bytes &key (compressed nil))
  "Encode a gRPC message with length-prefix framing.

   Args:
     message-bytes: Byte array of serialized protobuf message
     compressed: If T, mark as compressed (compression not yet implemented)

   Returns:
     Byte array with gRPC framing (5-byte header + message)

   Format:
     [0]     Compressed-Flag (0 or 1)
     [1-4]   Message-Length (big-endian uint32)
     [5...]  Message payload"
  (let* ((message-length (length message-bytes))
         (total-length (+ +grpc-message-header-size+ message-length))
         (result (make-byte-array total-length))
         (compressed-flag (if compressed +grpc-compressed-flag-gzip+ +grpc-compressed-flag-none+)))

    (when (> message-length +grpc-max-message-size+)
      (signal-grpc-resource-exhausted
       (format nil "Message too large: ~D bytes (max ~D)"
               message-length +grpc-max-message-size+)))

    ;; Compressed flag
    (setf (aref result 0) compressed-flag)

    ;; Message length (big-endian uint32)
    (encode-uint32-be message-length result 1)

    ;; Message payload
    (replace result message-bytes :start1 +grpc-message-header-size+)

    result))

(defun decode-grpc-message-header (bytes &optional (offset 0))
  "Decode gRPC message header (5 bytes).

   Returns:
     (values compressed-flag message-length)

   Signals grpc-error if header is invalid."
  (when (< (- (length bytes) offset) +grpc-message-header-size+)
    (signal-grpc-internal "Incomplete gRPC message header"))

  (let ((compressed-flag (aref bytes offset))
        (message-length (decode-uint32-be bytes (+ offset 1))))

    (unless (or (= compressed-flag +grpc-compressed-flag-none+)
                (= compressed-flag +grpc-compressed-flag-gzip+))
      (signal-grpc-internal
       (format nil "Invalid compressed flag: ~D" compressed-flag)))

    (when (> message-length +grpc-max-message-size+)
      (signal-grpc-resource-exhausted
       (format nil "Message too large: ~D bytes (max ~D)"
               message-length +grpc-max-message-size+)))

    (values compressed-flag message-length)))

(defun decode-grpc-message (bytes &optional (offset 0))
  "Decode a gRPC message from framed bytes.

   Args:
     bytes: Byte array containing framed gRPC message
     offset: Offset to start reading from (default 0)

   Returns:
     (values message-bytes compressed-flag total-bytes-read)

   The message-bytes are the raw protobuf payload (not yet deserialized).
   Caller must deserialize using appropriate protobuf message type."
  (multiple-value-bind (compressed-flag message-length)
      (decode-grpc-message-header bytes offset)

    (let* ((header-end (+ offset +grpc-message-header-size+))
           (message-end (+ header-end message-length)))

      (when (> message-end (length bytes))
        (signal-grpc-internal
         (format nil "Incomplete gRPC message: need ~D bytes, have ~D"
                 message-end (length bytes))))

      ;; TODO: Handle decompression if compressed-flag is set
      (when (= compressed-flag +grpc-compressed-flag-gzip+)
        (signal-grpc-unimplemented "Message compression not yet supported"))

      (let ((message-bytes (subseq bytes header-end message-end)))
        (values message-bytes compressed-flag (- message-end offset))))))

(defun read-grpc-message-from-stream (stream)
  "Read a gRPC message from a stream.

   Returns:
     (values message-bytes compressed-flag)

   Returns NIL if EOF reached before header."
  (let ((header (make-byte-array +grpc-message-header-size+)))

    ;; Read header
    (let ((bytes-read (read-sequence header stream)))
      (when (zerop bytes-read)
        ;; EOF
        (return-from read-grpc-message-from-stream nil))

      (when (< bytes-read +grpc-message-header-size+)
        (signal-grpc-internal "Incomplete gRPC message header")))

    ;; Parse header
    (multiple-value-bind (compressed-flag message-length)
        (decode-grpc-message-header header)

      ;; Read message payload
      (let ((message-bytes (make-byte-array message-length)))
        (let ((bytes-read (read-sequence message-bytes stream)))
          (when (< bytes-read message-length)
            (signal-grpc-internal
             (format nil "Incomplete gRPC message: expected ~D bytes, got ~D"
                     message-length bytes-read))))

        (values message-bytes compressed-flag)))))

(defun write-grpc-message-to-stream (stream message-bytes &key (compressed nil))
  "Write a gRPC message to a stream with framing.

   Args:
     stream: Output stream
     message-bytes: Serialized protobuf message
     compressed: If T, mark as compressed"
  (let ((framed-message (encode-grpc-message message-bytes :compressed compressed)))
    (write-sequence framed-message stream)
    (force-output stream)))

;;; Multi-message handling (for streaming RPCs - future phases)

(defun split-grpc-messages (bytes)
  "Split multiple gRPC messages from a byte array.

   Returns:
     List of message-bytes (each is raw protobuf payload)"
  (let ((messages nil)
        (offset 0))

    (loop while (and (< offset (length bytes))
                     (>= (- (length bytes) offset) +grpc-message-header-size+))
          do (multiple-value-bind (message-bytes compressed-flag bytes-read)
                 (decode-grpc-message bytes offset)
               (declare (ignore compressed-flag))
               (push message-bytes messages)
               (incf offset bytes-read)))

    (nreverse messages)))

(defun join-grpc-messages (message-list &key (compressed nil))
  "Join multiple gRPC messages into a single byte array.

   Args:
     message-list: List of message-bytes (serialized protobuf messages)
     compressed: If T, mark all messages as compressed

   Returns:
     Byte array containing all framed messages concatenated"
  (let* ((framed-messages (mapcar (lambda (msg)
                                    (encode-grpc-message msg :compressed compressed))
                                  message-list))
         (total-length (reduce #'+ framed-messages :key #'length))
         (result (make-byte-array total-length))
         (offset 0))

    (dolist (framed-msg framed-messages)
      (replace result framed-msg :start1 offset)
      (incf offset (length framed-msg)))

    result))
