;;;; client.lisp - Common Lisp gRPC client for interop testing
;;;;
;;;; This client calls a gRPC server (Go or CL) with a HelloWorld request.

(require :asdf)
(push (truename "../../../") asdf:*central-registry*)
(asdf:load-system :clgrpc)

(in-package #:cl-user)

(defun main ()
  "Run the HelloWorld client"
  (format t "~%Common Lisp gRPC Client~%")
  (format t "========================~%~%")

  ;; For now, this is a placeholder that demonstrates the structure
  ;; The actual implementation will require:
  ;; 1. Protobuf message serialization (using cl-protobufs)
  ;; 2. Creating a channel
  ;; 3. Making a unary RPC call

  (format t "Creating channel to localhost:50051...~%")

  (handler-case
      (let ((channel (clgrpc:make-channel "localhost:50051" :secure nil)))
        (format t "Channel created successfully!~%")
        (format t "~%")
        (format t "Note: Full implementation requires cl-protobufs integration~%")
        (format t "      for message serialization. This is a structural demo.~%")
        (clgrpc:close-channel channel))
    (error (e)
      (format t "Error: ~A~%" e)
      (return-from main nil)))

  (format t "~%Client test complete!~%"))

;; Run the client
(main)
