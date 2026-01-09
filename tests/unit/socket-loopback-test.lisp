;;;; Unit test: Socket read/write loopback

(require :asdf)
(push #P"/home/vydd/Code/clgrpc/" asdf:*central-registry*)
(asdf:load-system :clgrpc :verbose nil)

(format t "~%Testing socket loopback (write then read)...~%")

;; Start a simple echo server in background thread
(defvar *test-port* 50052)
(defvar *server-thread* nil)
(defvar *server-running* nil)

(setf *server-thread*
      (bordeaux-threads:make-thread
       (lambda ()
         (let ((server (clgrpc.transport:make-tcp-server *test-port*)))
           (setf *server-running* t)
           (format t "Echo server listening on port ~D~%" *test-port*)
           (handler-case
               (let ((client (clgrpc.transport:accept-connection server)))
                 (format t "Client connected~%")
                 ;; Read data and echo it back
                 (let ((data (clgrpc.transport:socket-read-bytes client 24)))
                   (format t "Server received ~D bytes~%" (length data))
                   (clgrpc.transport:socket-write-bytes client data)
                   (clgrpc.transport:socket-flush client)
                   (format t "Server echoed data back~%")
                   (sleep 0.1)
                   (clgrpc.transport:socket-close client)))
             (error (e)
               (format t "Server error: ~A~%" e)))
           (usocket:socket-close server)))))

;; Wait for server to start
(sleep 0.5)

;; Test client connection
(format t "~%Connecting client to echo server...~%")
(let ((client (clgrpc.transport:make-tcp-connection "localhost" *test-port*)))
  (format t "1. Client connected~%")

  ;; Send HTTP/2 preface
  (let ((preface (clgrpc.http2:http2-client-preface-bytes)))
    (format t "2. Sending ~D bytes...~%" (length preface))
    (clgrpc.transport:socket-write-bytes client preface)
    (clgrpc.transport:socket-flush client)
    (format t "3. Data sent~%")

    ;; Read echo back
    (let ((received (clgrpc.transport:socket-read-bytes client 24)))
      (format t "4. Received ~D bytes~%" (length received))
      (assert (equalp preface received) () "Echo data doesn't match!")
      (format t "   ✓ Echo matches sent data~%")))

  (clgrpc.transport:socket-close client))

(format t "~%✓ Socket loopback test passed~%")
