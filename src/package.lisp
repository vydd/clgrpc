;;;; package.lisp - Package definitions for clgrpc

(defpackage #:clgrpc.utils
  (:documentation "Utility functions for binary operations")
  (:use #:cl)
  (:export
   ;; Binary utilities
   #:encode-uint16-be
   #:encode-uint24-be
   #:encode-uint32-be
   #:decode-uint16-be
   #:decode-uint24-be
   #:decode-uint32-be
   #:make-byte-array
   #:copy-bytes))

(defpackage #:clgrpc.http2
  (:documentation "HTTP/2 protocol implementation")
  (:use #:cl #:clgrpc.utils)
  (:export
   ;; Error codes
   #:+http2-error-no-error+
   #:+http2-error-protocol-error+
   #:+http2-error-internal-error+
   #:+http2-error-flow-control-error+
   #:+http2-error-settings-timeout+
   #:+http2-error-stream-closed+
   #:+http2-error-frame-size-error+
   #:+http2-error-refused-stream+
   #:+http2-error-cancel+
   #:+http2-error-compression-error+
   #:+http2-error-connect-error+
   #:+http2-error-enhance-your-calm+
   #:+http2-error-inadequate-security+
   #:+http2-error-http-1-1-required+
   #:http2-error
   #:http2-protocol-error
   #:http2-flow-control-error
   #:http2-compression-error

   ;; Frame types
   #:+frame-type-data+
   #:+frame-type-headers+
   #:+frame-type-priority+
   #:+frame-type-rst-stream+
   #:+frame-type-settings+
   #:+frame-type-push-promise+
   #:+frame-type-ping+
   #:+frame-type-goaway+
   #:+frame-type-window-update+
   #:+frame-type-continuation+

   ;; Frame flags
   #:+flag-end-stream+
   #:+flag-end-headers+
   #:+flag-padded+
   #:+flag-priority+
   #:+flag-ack+

   ;; Frame constants
   #:+frame-header-size+
   #:+default-max-frame-size+
   #:+max-frame-size-limit+

   ;; Frame structure
   #:http2-frame
   #:frame-length
   #:frame-type
   #:frame-flags
   #:frame-stream-id
   #:frame-payload
   #:make-http2-frame
   #:frame-flag-set-p
   #:set-frame-flag

   ;; Frame encoding/decoding
   #:encode-frame
   #:decode-frame
   #:encode-frame-header
   #:decode-frame-header

   ;; Specific frame constructors
   #:make-data-frame
   #:make-headers-frame
   #:make-priority-frame
   #:make-rst-stream-frame
   #:make-settings-frame
   #:make-push-promise-frame
   #:make-ping-frame
   #:make-goaway-frame
   #:make-window-update-frame
   #:make-continuation-frame

   ;; Huffman coding
   #:huffman-encode
   #:huffman-decode
   #:huffman-encode-string
   #:huffman-decode-string

   ;; SETTINGS
   #:http2-settings
   #:make-http2-settings
   #:make-default-settings
   #:parse-settings-frame
   #:apply-settings
   #:make-settings-frame-from-settings
   #:build-settings-frame-payload
   #:http2-settings-header-table-size
   #:http2-settings-enable-push
   #:http2-settings-max-concurrent-streams
   #:http2-settings-initial-window-size
   #:http2-settings-max-frame-size
   #:http2-settings-max-header-list-size
   #:+settings-header-table-size+
   #:+settings-enable-push+
   #:+settings-max-concurrent-streams+
   #:+settings-initial-window-size+
   #:+settings-max-frame-size+
   #:+settings-max-header-list-size+

   ;; Flow Control
   #:flow-control-window
   #:make-flow-control-window
   #:window-available-p
   #:window-consume
   #:window-replenish
   #:window-size
   #:process-window-update
   #:+default-initial-window-size+

   ;; Stream
   #:http2-stream
   #:make-http2-stream
   #:stream-id
   #:stream-state
   #:stream-send-window
   #:stream-recv-window
   #:stream-send-headers
   #:stream-recv-headers
   #:stream-send-end-stream
   #:stream-recv-end-stream
   #:stream-send-rst-stream
   #:stream-add-received-data
   #:stream-add-received-headers
   #:stream-get-all-received-data
   #:stream-can-send-data-p
   #:stream-can-recv-data-p
   #:stream-is-closed-p
   #:stream-set-priority
   #:http2-stream-headers-received
   #:http2-stream-trailers-received
   #:http2-stream-priority-weight
   #:http2-stream-priority-depends-on
   #:http2-stream-priority-exclusive

   ;; Connection
   #:http2-connection
   #:make-http2-connection
   #:make-client-connection
   #:make-server-connection
   #:http2-client-preface-bytes
   #:connection-get-stream
   #:connection-create-stream
   #:connection-get-or-create-stream
   #:connection-allocate-stream-id
   #:connection-send-frame
   #:connection-read-frame
   #:connection-send-settings
   #:connection-send-settings-ack
   #:connection-send-window-update
   #:connection-send-ping
   #:connection-send-goaway
   #:connection-send-client-preface
   #:connection-receive-client-preface
   #:initialize-client-connection
   #:initialize-server-connection
   #:connection-is-closed-p
   #:connection-close
   #:http2-connection-is-client
   #:http2-connection-streams
   #:http2-connection-active-calls
   #:http2-connection-next-stream-id
   #:http2-connection-local-settings
   #:http2-connection-remote-settings
   #:http2-connection-hpack-encoder
   #:http2-connection-hpack-decoder
   #:http2-connection-socket
   #:http2-connection-closed
   #:http2-connection-goaway-sent
   #:http2-connection-goaway-received
   #:http2-connection-settings-received
   #:http2-connection-ready-lock
   #:http2-connection-ready-cv
   #:http2-connection-connection-send-window
   #:http2-connection-connection-recv-window
   #:http2-connection-goaway-sent
   #:http2-connection-goaway-received
   #:http2-connection-last-stream-id

   ;; Frame I/O
   #:read-frame-from-stream
   #:write-frame-to-stream
   #:write-frames-to-stream
   #:split-data-into-frames
   #:split-headers-into-frames
   #:read-continuation-frames
   #:combine-header-fragments

   ;; HPACK
   #:hpack-context
   #:make-hpack-context
   #:hpack-context-dynamic-table
   #:hpack-context-dynamic-table-size
   #:hpack-context-max-dynamic-table-size
   #:hpack-encode-header
   #:hpack-decode-header
   #:hpack-encode-headers
   #:hpack-decode-headers
   #:hpack-lookup-index
   #:hpack-find-index
   #:hpack-find-name-index
   #:hpack-dynamic-table-add
   #:hpack-encode-integer
   #:hpack-decode-integer
   #:hpack-encode-string
   #:hpack-decode-string
   #:hpack-encode-indexed
   #:hpack-encode-literal-with-indexing
   #:hpack-encode-literal-without-indexing))

(defpackage #:clgrpc.transport
  (:documentation "Transport layer (TCP/TLS/Buffered I/O) for gRPC")
  (:use #:cl #:clgrpc.utils)
  (:export
   ;; TCP socket operations
   #:tcp-socket
   #:tcp-socket-p
   #:make-tcp-connection
   #:make-tcp-server
   #:accept-connection
   #:socket-read-byte
   #:socket-read-bytes
   #:socket-read-sequence
   #:socket-write-byte
   #:socket-write-bytes
   #:socket-write-sequence
   #:socket-flush
   #:socket-close
   #:socket-open-p
   #:socket-wait-for-input
   #:socket-get-peer-info
   #:socket-set-timeout
   #:with-tcp-connection
   #:with-tcp-server

   ;; TLS socket operations
   #:tls-socket
   #:tls-socket-p
   #:make-tls-connection
   #:make-tls-server-context
   #:accept-tls-connection
   #:tls-read-byte
   #:tls-read-bytes
   #:tls-read-sequence
   #:tls-write-byte
   #:tls-write-bytes
   #:tls-write-sequence
   #:tls-flush
   #:tls-close
   #:tls-open-p
   #:tls-get-alpn-protocol
   #:tls-get-peer-info
   #:tls-get-peer-certificate
   #:with-tls-connection
   #:alpn-supported-p

   ;; Buffered socket operations
   #:buffered-socket
   #:buffered-socket-p
   #:wrap-socket-with-buffer
   #:buffered-read-byte
   #:buffered-read-bytes
   #:buffered-peek-bytes
   #:buffered-skip-bytes
   #:buffered-write-byte
   #:buffered-write-bytes
   #:buffered-flush
   #:buffered-close
   #:buffered-open-p
   #:buffered-get-underlying-socket
   #:with-buffered-socket
   #:buffered-read-available
   #:buffered-write-available))

(defpackage #:clgrpc.grpc
  (:documentation "gRPC protocol implementation")
  (:use #:cl #:clgrpc.utils)
  (:export
   ;; Status codes
   #:+grpc-status-ok+
   #:+grpc-status-cancelled+
   #:+grpc-status-unknown+
   #:+grpc-status-invalid-argument+
   #:+grpc-status-deadline-exceeded+
   #:+grpc-status-not-found+
   #:+grpc-status-already-exists+
   #:+grpc-status-permission-denied+
   #:+grpc-status-resource-exhausted+
   #:+grpc-status-failed-precondition+
   #:+grpc-status-aborted+
   #:+grpc-status-out-of-range+
   #:+grpc-status-unimplemented+
   #:+grpc-status-internal+
   #:+grpc-status-unavailable+
   #:+grpc-status-data-loss+
   #:+grpc-status-unauthenticated+
   #:valid-status-code-p
   #:status-code-name
   #:retryable-status-p

   ;; Errors
   #:grpc-error
   #:grpc-error-status-code
   #:grpc-error-message
   #:grpc-error-details
   #:grpc-cancelled
   #:grpc-invalid-argument
   #:grpc-deadline-exceeded
   #:grpc-not-found
   #:grpc-already-exists
   #:grpc-permission-denied
   #:grpc-resource-exhausted
   #:grpc-failed-precondition
   #:grpc-aborted
   #:grpc-out-of-range
   #:grpc-unimplemented
   #:grpc-internal
   #:grpc-unavailable
   #:grpc-data-loss
   #:grpc-unauthenticated
   #:make-grpc-error
   #:signal-grpc-error
   #:signal-grpc-cancelled
   #:signal-grpc-invalid-argument
   #:signal-grpc-deadline-exceeded
   #:signal-grpc-not-found
   #:signal-grpc-already-exists
   #:signal-grpc-permission-denied
   #:signal-grpc-resource-exhausted
   #:signal-grpc-failed-precondition
   #:signal-grpc-aborted
   #:signal-grpc-out-of-range
   #:signal-grpc-unimplemented
   #:signal-grpc-internal
   #:signal-grpc-unavailable
   #:signal-grpc-data-loss
   #:signal-grpc-unauthenticated

   ;; Protocol
   #:+grpc-compressed-flag-none+
   #:+grpc-compressed-flag-gzip+
   #:+grpc-message-header-size+
   #:+grpc-max-message-size+
   #:encode-grpc-message
   #:decode-grpc-message
   #:decode-grpc-message-header
   #:read-grpc-message-from-stream
   #:write-grpc-message-to-stream
   #:split-grpc-messages
   #:join-grpc-messages

   ;; Metadata
   #:+grpc-content-type+
   #:+grpc-te+
   #:metadata-key-binary-p
   #:encode-metadata
   #:decode-metadata
   #:encode-grpc-timeout
   #:decode-grpc-timeout
   #:encode-grpc-request-headers
   #:encode-grpc-response-headers
   #:encode-grpc-trailers
   #:decode-grpc-trailers
   #:grpc-content-type-p

   ;; Message
   #:serialize-message
   #:deserialize-message
   #:serialize-message-impl
   #:deserialize-message-impl
   #:encode-grpc-request-message
   #:decode-grpc-request-message
   #:encode-grpc-response-message
   #:decode-grpc-response-message
   #:simple-text-message
   #:message-text
   #:make-simple-text-message

   ;; HelloWorld messages (CLOS-based)
   #:hello-request
   #:make-hello-request
   #:hello-request-name
   #:hello-reply
   #:make-hello-reply
   #:hello-reply-message
   ;; Backward compatibility (deprecated)
   #:encode-hello-request
   #:decode-hello-request
   #:encode-hello-reply
   #:decode-hello-reply

   ;; Reflection Protocol
   #:encode-reflection-request-list-services
   #:decode-reflection-request
   #:encode-reflection-response-list-services

   ;; Protobuf Code Generation
   #:compile-proto-file
   #:generate-code-from-proto

   ;; CLOS-based Protobuf API
   #:proto-metaclass
   #:proto-message
   #:proto-serialize
   #:proto-deserialize
   #:proto-serialize-grpc
   #:proto-deserialize-grpc

   ;; CLOS-based Service API
   #:grpc-service
   #:grpc-service-metaclass
   #:defgrpc-method
   #:get-service-name
   #:get-service-package
   #:grpc-method-info
   #:grpc-method-info-lisp-name
   #:grpc-method-info-grpc-name
   #:grpc-method-info-rpc-type
   #:grpc-method-info-request-type
   #:grpc-method-info-response-type
   #:list-grpc-methods
   #:find-grpc-method
   #:find-grpc-method-by-name

   ;; Low-level Protobuf Wire Format
   ;; Wire types
   #:+wire-type-varint+
   #:+wire-type-i64+
   #:+wire-type-length-delimited+
   #:+wire-type-i32+

   ;; Encoding functions
   #:encode-varint
   #:encode-int32-field
   #:encode-int64-field
   #:encode-uint32-field
   #:encode-uint64-field
   #:encode-sint32-field
   #:encode-sint64-field
   #:encode-bool-field
   #:encode-fixed32-field
   #:encode-fixed64-field
   #:encode-sfixed32-field
   #:encode-sfixed64-field
   #:encode-float-field
   #:encode-double-field
   #:encode-string-field
   #:encode-bytes-field
   #:encode-message-field
   #:encode-message

   ;; Decoding functions
   #:decode-field-tag
   #:decode-varint
   #:decode-int32
   #:decode-int64
   #:decode-uint32
   #:decode-uint64
   #:decode-sint32
   #:decode-sint64
   #:decode-bool
   #:decode-fixed32
   #:decode-fixed64
   #:decode-sfixed32
   #:decode-sfixed64
   #:proto-decode-float
   #:proto-decode-double
   #:decode-string-field
   #:decode-bytes-field
   #:decode-length-delimited
   #:skip-field))

(defpackage #:clgrpc.client
  (:documentation "gRPC client implementation")
  (:use #:cl #:clgrpc.utils #:clgrpc.grpc #:clgrpc.http2 #:clgrpc.transport)
  (:export
   ;; Client API
   #:grpc-channel
   #:make-channel
   #:close-channel
   #:call-unary

   ;; Streaming API
   #:call-client-streaming
   #:call-server-streaming
   #:call-bidirectional-streaming
   #:grpc-stream
   #:create-stream
   #:stream-start
   #:stream-send
   #:stream-recv
   #:stream-close-send

   ;; Client call
   #:client-call
   #:send-message
   #:receive-message
   #:close-send))

(defpackage #:clgrpc.server
  (:documentation "gRPC server implementation")
  (:use #:cl #:clgrpc.utils #:clgrpc.grpc #:clgrpc.http2 #:clgrpc.transport)
  (:export
   ;; Server API
   #:grpc-server
   #:grpc-server-router
   #:make-server
   #:start-server
   #:stop-server
   #:register-service

   ;; Router
   #:register-handler
   #:unregister-handler

   ;; Handler interface
   #:handle-unary
   #:handle-client-streaming
   #:handle-server-streaming
   #:handle-bidirectional-streaming
   #:handler-context
   #:get-stream

   ;; Server streaming operations
   #:grpc-server-stream
   #:server-stream-send
   #:server-stream-recv

   ;; Reflection service
   #:register-reflection-service

   ;; Service definition
   #:define-grpc-service))

(defpackage #:clgrpc
  (:documentation "Main clgrpc package - re-exports public API")
  (:use #:cl)
  (:import-from #:clgrpc.client
                #:make-channel
                #:close-channel
                #:call-unary)
  (:import-from #:clgrpc.server
                #:make-server
                #:start-server
                #:stop-server
                #:register-service
                #:define-grpc-service)
  (:import-from #:clgrpc.grpc
                #:grpc-error
                #:grpc-error-status-code
                #:grpc-error-message
                #:+grpc-status-ok+
                #:+grpc-status-cancelled+
                #:+grpc-status-unknown+
                #:+grpc-status-invalid-argument+
                #:+grpc-status-deadline-exceeded+
                #:+grpc-status-not-found+
                #:+grpc-status-already-exists+
                #:+grpc-status-permission-denied+
                #:+grpc-status-resource-exhausted+
                #:+grpc-status-failed-precondition+
                #:+grpc-status-aborted+
                #:+grpc-status-out-of-range+
                #:+grpc-status-unimplemented+
                #:+grpc-status-internal+
                #:+grpc-status-unavailable+
                #:+grpc-status-data-loss+
                #:+grpc-status-unauthenticated+)
  (:export
   ;; Client API
   #:make-channel
   #:close-channel
   #:call-unary

   ;; Server API
   #:make-server
   #:start-server
   #:stop-server
   #:register-service
   #:define-grpc-service

   ;; Errors
   #:grpc-error
   #:grpc-error-status-code
   #:grpc-error-message

   ;; Status codes
   #:+grpc-status-ok+
   #:+grpc-status-cancelled+
   #:+grpc-status-unknown+
   #:+grpc-status-invalid-argument+
   #:+grpc-status-deadline-exceeded+
   #:+grpc-status-not-found+
   #:+grpc-status-already-exists+
   #:+grpc-status-permission-denied+
   #:+grpc-status-resource-exhausted+
   #:+grpc-status-failed-precondition+
   #:+grpc-status-aborted+
   #:+grpc-status-out-of-range+
   #:+grpc-status-unimplemented+
   #:+grpc-status-internal+
   #:+grpc-status-unavailable+
   #:+grpc-status-data-loss+
   #:+grpc-status-unauthenticated+))
