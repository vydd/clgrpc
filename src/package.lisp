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

   ;; Errors
   #:grpc-error
   #:grpc-error-status-code
   #:grpc-error-message
   #:grpc-error-details

   ;; Protocol
   #:encode-grpc-message
   #:decode-grpc-message

   ;; Metadata
   #:encode-metadata
   #:decode-metadata
   #:encode-grpc-request-headers
   #:encode-grpc-response-headers
   #:decode-grpc-trailers))

(defpackage #:clgrpc.client
  (:documentation "gRPC client implementation")
  (:use #:cl #:clgrpc.grpc #:clgrpc.http2)
  (:export
   ;; Client API
   #:grpc-channel
   #:make-channel
   #:close-channel
   #:call-unary

   ;; Client call
   #:client-call
   #:send-message
   #:receive-message
   #:close-send))

(defpackage #:clgrpc.server
  (:documentation "gRPC server implementation")
  (:use #:cl #:clgrpc.grpc #:clgrpc.http2)
  (:export
   ;; Server API
   #:grpc-server
   #:make-server
   #:start-server
   #:stop-server
   #:register-service

   ;; Handler interface
   #:handle-unary-call

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
