;;;; clgrpc-examples.asd - ASDF system definition for clgrpc examples

(asdf:defsystem #:clgrpc-examples
  :description "Examples for clgrpc - Common Lisp gRPC library"
  :version "0.1.0"
  :author "clgrpc contributors"
  :license "MIT"
  :depends-on (#:clgrpc
               #:cl-json)  ; For routeguide JSON database
  :pathname "examples"
  :serial t
  :components ((:file "package")
               ;; HelloWorld examples
               (:module "helloworld"
                :components
                ((:file "client-clos")
                 (:file "server-clos")))
               ;; RouteGuide examples (demonstrates all RPC types)
               (:module "routeguide"
                :components
                ((:file "routeguide-proto")
                 (:file "client")
                 (:file "server")
                 (:file "server-clos")))))
