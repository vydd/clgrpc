;;;; clgrpc-tests.asd - ASDF system definition for clgrpc tests

(defsystem "clgrpc-tests"
  :description "Test suite for clgrpc"
  :version "0.1.0"
  :author "clgrpc contributors"
  :license "MIT"
  :depends-on (#:clgrpc
               #:fiveam)
  :pathname "tests"
  :serial t
  :components
  ((:file "package")

   (:module "http2"
    :components
    ((:file "frame-tests")
     (:file "huffman-tests")
     (:file "hpack-tests")
     (:file "stream-tests")
     (:file "connection-tests")))

   (:module "grpc"
    :components
    ((:file "protocol-tests")))

   (:module "client"
    :components
    ((:file "client-tests")))

   (:module "server"
    :components
    ((:file "server-tests")))

   (:module "interop"
    :components
    ((:file "interop-tests")))

   (:module "integration"
    :components
    ((:file "echo-test"))))

  :perform (test-op (o c) (symbol-call :fiveam :run! :clgrpc-all)))
