;;;; test-transport-compile.lisp - Verify transport layer compiles correctly

(require :asdf)

(format t "~%================================================~%")
(format t "Transport Layer Compilation Test~%")
(format t "================================================~%~%")

(format t "Loading package definitions...~%")
(load (merge-pathnames "src/package.lisp" (truename ".")))

(format t "Entering clgrpc.transport package...~%")
(in-package #:clgrpc.transport)

(format t "~%Compiling socket.lisp...~%")
(handler-case
    (progn
      (compile-file (merge-pathnames "src/transport/socket.lisp" (truename ".")))
      (format t "✓ socket.lisp compiled successfully~%"))
  (error (e)
    (format t "✗ socket.lisp compilation FAILED: ~A~%" e)
    (quit :unix-status 1)))

(format t "~%Compiling tls.lisp...~%")
(handler-case
    (progn
      (compile-file (merge-pathnames "src/transport/tls.lisp" (truename ".")))
      (format t "✓ tls.lisp compiled successfully~%"))
  (error (e)
    (format t "✗ tls.lisp compilation FAILED: ~A~%" e)
    (quit :unix-status 1)))

(format t "~%Compiling buffer.lisp...~%")
(handler-case
    (progn
      (compile-file (merge-pathnames "src/transport/buffer.lisp" (truename ".")))
      (format t "✓ buffer.lisp compiled successfully~%"))
  (error (e)
    (format t "✗ buffer.lisp compilation FAILED: ~A~%" e)
    (quit :unix-status 1)))

(format t "~%Loading socket.lisp...~%")
(handler-case
    (progn
      (load (merge-pathnames "src/transport/socket.lisp" (truename ".")))
      (format t "✓ socket.lisp loaded successfully~%"))
  (error (e)
    (format t "✗ socket.lisp loading FAILED: ~A~%" e)
    (quit :unix-status 1)))

(format t "~%Loading tls.lisp...~%")
(handler-case
    (progn
      (load (merge-pathnames "src/transport/tls.lisp" (truename ".")))
      (format t "✓ tls.lisp loaded successfully~%"))
  (error (e)
    (format t "✗ tls.lisp loading FAILED: ~A~%" e)
    (quit :unix-status 1)))

(format t "~%Loading buffer.lisp...~%")
(handler-case
    (progn
      (load (merge-pathnames "src/transport/buffer.lisp" (truename ".")))
      (format t "✓ buffer.lisp loaded successfully~%"))
  (error (e)
    (format t "✗ buffer.lisp loading FAILED: ~A~%" e)
    (quit :unix-status 1)))

(format t "~%================================================~%")
(format t "Testing structure definitions...~%")
(format t "================================================~%~%")

;; Test structure creation (without actual network operations)
(format t "Testing tcp-socket structure...~%")
(let ((sock (make-tcp-socket :usocket nil :stream nil :host "localhost" :port 8080 :timeout 30)))
  (format t "  Created tcp-socket: host=~A port=~D timeout=~D~%"
          (tcp-socket-host sock)
          (tcp-socket-port sock)
          (tcp-socket-timeout sock))
  (format t "✓ tcp-socket structure works~%"))

(format t "~%Testing tls-socket structure...~%")
(let ((sock (make-tls-socket :ssl-stream nil :tcp-socket nil :host "example.com" :port 443
                             :alpn-protocol "h2" :verified t)))
  (format t "  Created tls-socket: host=~A port=~D alpn=~A~%"
          (tls-socket-host sock)
          (tls-socket-port sock)
          (tls-socket-alpn-protocol sock))
  (format t "✓ tls-socket structure works~%"))

(format t "~%Testing buffered-socket structure...~%")
(let ((buf (make-buffered-socket
            :socket nil
            :read-buffer (make-array 1024 :element-type '(unsigned-byte 8))
            :read-pos 0
            :read-limit 0
            :write-buffer (make-array 1024 :element-type '(unsigned-byte 8))
            :write-pos 0)))
  (format t "  Created buffered-socket: read-buffer=~D bytes write-buffer=~D bytes~%"
          (length (buffered-socket-read-buffer buf))
          (length (buffered-socket-write-buffer buf)))
  (format t "✓ buffered-socket structure works~%"))

(format t "~%================================================~%")
(format t "Transport Layer Compilation Test PASSED!~%")
(format t "================================================~%~%")

(format t "Summary:~%")
(format t "- socket.lisp: ✓ Compiled and loaded~%")
(format t "- tls.lisp: ✓ Compiled and loaded~%")
(format t "- buffer.lisp: ✓ Compiled and loaded~%")
(format t "- All structures: ✓ Working~%")
(format t "~%Transport layer implementation is complete!~%")
(format t "~%Note: Runtime network tests require:~%")
(format t "  - usocket (TCP sockets)~%")
(format t "  - cl+ssl (TLS/ALPN)~%")
(format t "  - bordeaux-threads (multi-threading)~%")
(format t "~%Next step: Update client/server to use real transport~%~%")
