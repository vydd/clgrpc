;;;; package.lisp - Package definition for clgrpc examples

(defpackage #:clgrpc-examples
  (:use #:cl)
  ;; Import from clgrpc.client
  (:import-from #:clgrpc.client
                #:make-channel
                #:close-channel
                #:call-unary
                #:call-server-streaming
                #:call-client-streaming
                #:call-bidirectional-streaming
                #:stream-send
                #:stream-recv
                #:stream-close-send)
  ;; Import from clgrpc.server
  (:import-from #:clgrpc.server
                #:make-server
                #:start-server
                #:stop-server
                #:register-service
                #:register-handler
                #:server-stream-send
                #:server-stream-recv
                #:grpc-server-router
                #:handle-unary
                #:handle-server-streaming
                #:handle-client-streaming
                #:handle-bidirectional-streaming)
  ;; Import from clgrpc.grpc
  (:import-from #:clgrpc.grpc
                #:+grpc-status-ok+
                #:+grpc-status-unimplemented+
                #:grpc-error
                #:grpc-error-message
                #:grpc-error-status-code
                #:proto-serialize
                #:proto-deserialize
                #:proto-message
                #:proto-metaclass
                #:grpc-service
                #:grpc-service-metaclass
                #:defgrpc-method
                #:make-hello-request
                #:make-hello-reply
                #:hello-request
                #:hello-reply
                #:hello-request-name
                #:hello-reply-message)
  (:export ;; HelloWorld example functions
           #:helloworld-client-main
           #:helloworld-server-main
           ;; RouteGuide example functions
           #:routeguide-client-main
           #:routeguide-server-main
           #:routeguide-server-clos-main))
