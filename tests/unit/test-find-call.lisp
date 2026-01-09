;;;; Test find-call-by-stream-id directly

(require :asdf)
(push #P"/home/vydd/Code/clgrpc/" asdf:*central-registry*)
(asdf:load-system :clgrpc :verbose nil)

(format t "~%Testing find-call-by-stream-id...~%")

(let* ((conn (clgrpc.http2:make-http2-connection
               :socket nil
               :is-client t))
       (call :dummy-call))
  (format t "1. Created connection with hash table~%")
  (format t "2. Registering call on stream 1...~%")
  (setf (gethash 1 (clgrpc.http2:http2-connection-active-calls conn)) call)
  (format t "3. Hash table count: ~D~%"
          (hash-table-count (clgrpc.http2:http2-connection-active-calls conn)))
  (format t "4. Direct gethash for stream 1: ~A~%"
          (gethash 1 (clgrpc.http2:http2-connection-active-calls conn)))
  (format t "5. Calling find-call-by-stream-id for stream 1...~%")
  (let ((result (clgrpc.client::find-call-by-stream-id conn 1)))
    (format t "6. Result: ~A~%" result)
    (if result
        (format t "~%✓ SUCCESS: find-call works!~%")
        (format t "~%✗ FAIL: find-call returned NIL~%"))))
