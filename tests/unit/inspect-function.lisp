;;;; Inspect the find-call-by-stream-id function

(require :asdf)
(push #P"/home/vydd/Code/clgrpc/" asdf:*central-registry*)
(asdf:load-system :clgrpc :verbose nil)

(format t "~%Function definition:~%")
(format t "~A~%" (function-lambda-expression #'clgrpc.client::find-call-by-stream-id))

(format t "~%Source location:~%")
(format t "~A~%" (sb-introspect:find-definition-source #'clgrpc.client::find-call-by-stream-id))

(format t "~%Testing direct call:~%")
(let* ((conn (make-instance 'clgrpc.http2:http2-connection))
       (ht (make-hash-table)))
  (setf (clgrpc.http2:http2-connection-active-calls conn) ht)
  (setf (gethash 99 ht) :test-call)
  (format t "Direct gethash result: ~A~%" (gethash 99 ht))
  (format t "find-call result: ~A~%" (clgrpc.client::find-call-by-stream-id conn 99)))
