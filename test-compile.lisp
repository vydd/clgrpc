;;;; test-compile.lisp - Test compilation of entire clgrpc system

(format t "~%================================================~%")
(format t "clgrpc Compilation Test~%")
(format t "================================================~%~%")

(format t "Loading package definitions...~%")
(handler-case
    (load "src/package.lisp")
  (error (e)
    (format t "✗ FAILED to load package.lisp: ~A~%~%" e)
    (quit :unix-status 1)))
(format t "✓ Package definitions loaded~%~%")

(format t "Testing package accessibility...~%")
(handler-case
    (progn
      (in-package #:clgrpc.utils)
      (format t "✓ clgrpc.utils package exists~%")
      (in-package #:clgrpc.http2)
      (format t "✓ clgrpc.http2 package exists~%")
      (in-package #:clgrpc.grpc)
      (format t "✓ clgrpc.grpc package exists~%")
      (in-package #:clgrpc.transport)
      (format t "✓ clgrpc.transport package exists~%")
      (in-package #:clgrpc.client)
      (format t "✓ clgrpc.client package exists~%")
      (in-package #:clgrpc.server)
      (format t "✓ clgrpc.server package exists~%"))
  (error (e)
    (format t "✗ FAILED package test: ~A~%~%" e)
    (quit :unix-status 1)))

(format t "~%Loading source files in dependency order...~%~%")

(defvar *load-errors* nil)
(defvar *load-count* 0)

(defun try-load (file description)
  "Try to load a file and report success/failure."
  (format t "~3D. ~A..." (incf *load-count*) description)
  (force-output)
  (handler-case
      (progn
        (load file)
        (format t " ✓~%")
        t)
    (error (e)
      (format t " ✗~%")
      (format t "     ERROR: ~A~%~%" e)
      (push (list file description e) *load-errors*)
      nil)))

;; Load in dependency order
(try-load "src/utils/binary-utils.lisp" "Binary utilities")

;; HTTP/2 layer
(try-load "src/http2/errors.lisp" "HTTP/2 errors")
(try-load "src/http2/frames.lisp" "HTTP/2 frames")
(try-load "src/http2/huffman.lisp" "Huffman coding")
(try-load "src/http2/hpack.lisp" "HPACK compression")
(try-load "src/http2/settings.lisp" "SETTINGS handling")
(try-load "src/http2/flow-control.lisp" "Flow control")
(try-load "src/http2/stream.lisp" "Stream state machine")
(try-load "src/http2/connection.lisp" "Connection management")
(try-load "src/http2/frame-reader.lisp" "Frame reading")
(try-load "src/http2/frame-writer.lisp" "Frame writing")

;; Transport layer
(try-load "src/transport/socket.lisp" "TCP sockets")
(try-load "src/transport/tls.lisp" "TLS with ALPN")
(try-load "src/transport/buffer.lisp" "Buffered I/O")

;; Protocol Buffers
(try-load "src/grpc/protobuf-simple.lisp" "Protobuf wire format")
(try-load "src/grpc/protobuf.lisp" "Protobuf advanced")
(try-load "src/grpc/protobuf-codegen.lisp" "Protobuf code generator")

;; gRPC protocol
(try-load "src/grpc/status.lisp" "gRPC status codes")
(try-load "src/grpc/errors.lisp" "gRPC errors")
(try-load "src/grpc/protocol.lisp" "gRPC protocol")
(try-load "src/grpc/metadata.lisp" "gRPC metadata")

;; Client
(try-load "src/client/stub.lisp" "Client stubs")
(try-load "src/client/call.lisp" "Client call")
(try-load "src/client/connection-pool.lisp" "Connection pool")
(try-load "src/client/client.lisp" "Client API")

;; Server
(try-load "src/server/handler.lisp" "Server handler")
(try-load "src/server/router.lisp" "Server router")
(try-load "src/server/service.lisp" "Server service")
(try-load "src/server/server.lisp" "Server API")

(format t "~%================================================~%")
(format t "Compilation Test Results~%")
(format t "================================================~%~%")

(format t "Total files attempted: ~D~%" *load-count*)
(format t "Successfully loaded: ~D~%" (- *load-count* (length *load-errors*)))
(format t "Failed to load: ~D~%~%" (length *load-errors*))

(when *load-errors*
  (format t "ERRORS:~%")
  (format t "-------~%~%")
  (loop for (file description error) in (reverse *load-errors*)
        for i from 1
        do (format t "~D. ~A (~A)~%" i description file)
           (format t "   ~A~%~%" error))
  (format t "~%================================================~%")
  (format t "Compilation FAILED - ~D errors~%"  (length *load-errors*))
  (format t "================================================~%~%")
  (quit :unix-status 1))

(format t "================================================~%")
(format t "Compilation SUCCESSFUL! All files loaded. ✓~%")
(format t "================================================~%~%")

(format t "Next steps:~%")
(format t "1. Check for missing function exports~%")
(format t "2. Test basic functionality~%")
(format t "3. Run integration tests~%")
(format t "4. Try Go interop tests~%~%")
