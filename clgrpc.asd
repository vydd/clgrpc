;;;; clgrpc.asd - ASDF system definition for Common Lisp gRPC library

(defsystem "clgrpc"
  :description "Pure Common Lisp gRPC implementation with HTTP/2"
  :version "0.1.0"
  :author "clgrpc contributors"
  :license "MIT"
  :depends-on (;; #:cl-protobufs  ; Not in Quicklisp - will add when available
               #:cl+ssl
               #:usocket
               #:bordeaux-threads
               #:alexandria
               #:trivial-gray-streams
               #:fast-io
               #:babel
               ) ;; ieee-floats not needed - we implement float encoding ourselves
  :pathname "src"
  :serial t
  :components
  ((:file "package")

   (:module "utils"
    :components
    ((:file "binary-utils")))

   (:module "http2"
    :components
    ((:file "errors")
     (:file "huffman")
     (:file "hpack" :depends-on ("huffman" "errors"))
     (:file "frames" :depends-on ("errors"))
     (:file "settings" :depends-on ("frames" "errors"))
     (:file "flow-control" :depends-on ("errors"))
     (:file "stream" :depends-on ("frames" "flow-control" "errors" "settings"))
     (:file "frame-reader" :depends-on ("frames"))
     (:file "frame-writer" :depends-on ("frames"))
     (:file "connection" :depends-on ("stream" "frames" "frame-reader" "frame-writer" "hpack" "settings"))))

   (:module "grpc"
    :components
    ((:file "status")
     (:file "errors")
     (:file "metadata")
     (:file "message")
     (:file "protocol")
     (:file "protobuf-simple")
     (:file "protobuf" :depends-on ("protobuf-simple"))
     (:file "helloworld")))

   (:module "transport"
    :components
    ((:file "buffer")
     (:file "socket")
     (:file "tls")))

   (:module "client"
    :components
    ((:file "call")
     (:file "connection-pool")
     (:file "client")
     (:file "stub")))

   (:module "server"
    :components
    ((:file "handler")
     (:file "router")
     (:file "service")
     (:file "server"))))

  :in-order-to ((test-op (test-op "clgrpc-tests"))))
