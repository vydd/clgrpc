;;;; test-codegen.lisp - Test protobuf code generator

(require :asdf)

;; Load Quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Load babel for string encoding
(asdf:load-system :babel)

;; Load package definitions and protobuf files
(load (merge-pathnames "src/package.lisp" (truename ".")))
(in-package #:clgrpc.grpc)
(load (merge-pathnames "src/grpc/protobuf-simple.lisp" (truename ".")))
(load (merge-pathnames "src/grpc/protobuf.lisp" (truename ".")))
(load (merge-pathnames "src/grpc/protobuf-codegen.lisp" (truename ".")))

(format t "~%================================================~%")
(format t "Protobuf Code Generator Test~%")
(format t "================================================~%~%")

(format t "Compiling test.proto...~%~%")

(let ((generated-code (compile-proto-file "tests/test.proto")))
  (format t "Generated code:~%")
  (format t "~A~%" generated-code)

  ;; Write to output file
  (with-open-file (out "tests/test-generated.lisp"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-string "(in-package #:clgrpc.grpc)" out)
    (terpri out)
    (terpri out)
    (write-string generated-code out))

  (format t "~%Written to tests/test-generated.lisp~%")
  (format t "~%================================================~%")
  (format t "Code generation SUCCESSFUL! ✓~%")
  (format t "================================================~%~%"))
