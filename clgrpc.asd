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
               #:babel)
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
     (:file "frames")))

   (:module "grpc"
    :components
    ((:file "status")
     (:file "errors")
     (:file "metadata")
     (:file "message")
     (:file "protocol")))

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
