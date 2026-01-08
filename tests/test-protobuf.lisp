;;;; test-protobuf.lisp - Test protobuf encoding/decoding

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

(defpackage #:test-protobuf
  (:use #:cl))

(in-package #:test-protobuf)

(defun test-varint ()
  "Test varint encoding/decoding"
  (format t "Testing varint encoding...~%")
  (let ((test-values '(0 1 127 128 255 256 16383 16384 2097151 2097152)))
    (dolist (val test-values)
      (let* ((encoded (clgrpc.grpc::encode-varint val))
             (decoded (clgrpc.grpc::decode-varint encoded)))
        (format t "  ~8D -> ~A bytes -> ~8D ~A~%"
                val (length encoded) decoded
                (if (= val decoded) "✓" "✗"))
        (unless (= val decoded)
          (error "Varint test failed for ~D" val))))))

(defun test-zigzag ()
  "Test zigzag encoding for signed integers"
  (format t "~%Testing zigzag encoding...~%")
  (let ((test-values '(0 -1 1 -2 2 -64 64 -8192 8192)))
    (dolist (val test-values)
      (let* ((encoded (clgrpc.grpc::encode-zigzag-32 val))
             (decoded (clgrpc.grpc::decode-zigzag-32 encoded)))
        (format t "  ~8D -> ~8D -> ~8D ~A~%"
                val encoded decoded
                (if (= val decoded) "✓" "✗"))
        (unless (= val decoded)
          (error "Zigzag test failed for ~D" val))))))

(defun test-fixed ()
  "Test fixed-width integer encoding"
  (format t "~%Testing fixed-width integers...~%")
  (let ((test-values-32 '(0 1 255 256 65535 65536 #xFFFFFFFF))
        (test-values-64 '(0 1 #xFFFFFFFF #x100000000 #xFFFFFFFFFFFFFFFF)))

    (format t "  Fixed32:~%")
    (dolist (val test-values-32)
      (let* ((encoded (clgrpc.grpc::encode-fixed32 val))
             (decoded (clgrpc.grpc::decode-fixed32 encoded 0)))
        (format t "    ~16D -> ~D bytes -> ~16D ~A~%"
                val (length encoded) decoded
                (if (= val decoded) "✓" "✗"))
        (unless (= val decoded)
          (error "Fixed32 test failed for ~D" val))))

    (format t "  Fixed64:~%")
    (dolist (val test-values-64)
      (let* ((encoded (clgrpc.grpc::encode-fixed64 val))
             (decoded (clgrpc.grpc::decode-fixed64 encoded 0)))
        (format t "    ~20D -> ~D bytes -> ~20D ~A~%"
                val (length encoded) decoded
                (if (= val decoded) "✓" "✗"))
        (unless (= val decoded)
          (error "Fixed64 test failed for ~D" val))))))

(defun test-float ()
  "Test float encoding"
  (format t "~%Testing float/double encoding...~%")
  (let ((test-floats '(0.0 1.0 -1.0 3.14159 -2.71828 1.0e10 -1.0e-10))
        (test-doubles '(0.0d0 1.0d0 -1.0d0 3.14159265358979d0 -2.71828182845905d0 1.0d100)))

    (format t "  Float:~%")
    (dolist (val test-floats)
      (let* ((encoded (clgrpc.grpc::pb-encode-float val))
             (decoded (clgrpc.grpc::pb-decode-float encoded 0)))
        (format t "    ~15F -> ~D bytes -> ~15F ~A~%"
                val (length encoded) decoded
                (if (< (abs (- val decoded)) 1.0e-6) "✓" "✗"))
        (unless (< (abs (- val decoded)) 1.0e-6)
          (error "Float test failed for ~F" val))))

    (format t "  Double:~%")
    (dolist (val test-doubles)
      (let* ((encoded (clgrpc.grpc::pb-encode-double val))
             (decoded (clgrpc.grpc::pb-decode-double encoded 0)))
        (format t "    ~20E -> ~D bytes -> ~20E ~A~%"
                val (length encoded) decoded
                (if (< (abs (- val decoded)) 1.0d-10) "✓" "✗"))
        (unless (< (abs (- val decoded)) 1.0d-10)
          (error "Double test failed for ~F" val))))))

(defun test-string ()
  "Test string encoding"
  (format t "~%Testing string encoding...~%")
  (let ((test-strings '("" "Hello" "Hello, World!" "Unicode: 你好世界")))
    (dolist (str test-strings)
      (let* ((encoded (clgrpc.grpc::encode-string-field 1 str))
             (decoded (clgrpc.grpc::decode-hello-request encoded)))
        (format t "  ~S -> ~D bytes -> ~S ~A~%"
                str (length encoded) decoded
                (if (string= str decoded) "✓" "✗"))
        (unless (string= str decoded)
          (error "String test failed for ~S" str))))))

(defun test-message ()
  "Test complete message encoding (HelloWorld)"
  (format t "~%Testing HelloWorld messages...~%")
  (let ((test-names '("Alice" "Bob" "世界")))
    (dolist (name test-names)
      (let* ((request-encoded (clgrpc.grpc:encode-hello-request name))
             (request-decoded (clgrpc.grpc:decode-hello-request request-encoded))
             (reply-msg (format nil "Hello ~A" name))
             (reply-encoded (clgrpc.grpc:encode-hello-reply reply-msg))
             (reply-decoded (clgrpc.grpc:decode-hello-reply reply-encoded)))

        (format t "  HelloRequest(~S) -> ~D bytes -> ~S ~A~%"
                name (length request-encoded) request-decoded
                (if (string= name request-decoded) "✓" "✗"))
        (format t "  HelloReply(~S) -> ~D bytes -> ~S ~A~%"
                reply-msg (length reply-encoded) reply-decoded
                (if (string= reply-msg reply-decoded) "✓" "✗"))

        (unless (and (string= name request-decoded)
                    (string= reply-msg reply-decoded))
          (error "Message test failed"))))))

(defun run-all-tests ()
  "Run all protobuf tests"
  (format t "~%================================================~%")
  (format t "Protobuf Implementation Tests~%")
  (format t "================================================~%~%")

  (handler-case
      (progn
        (test-varint)
        (test-zigzag)
        (test-fixed)
        (test-float)
        (test-string)
        (test-message)

        (format t "~%================================================~%")
        (format t "All tests PASSED! ✓✓✓~%")
        (format t "================================================~%~%")
        t)
    (error (e)
      (format t "~%~%TEST FAILED: ~A~%~%" e)
      nil)))

;; Run tests
(run-all-tests)
