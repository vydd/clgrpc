;;;; stub.lisp - Client stub generation for gRPC services
;;;;
;;;; Provides automatic client stub generation from:
;;;; 1. CLOS service definitions (introspection)
;;;; 2. .proto files (code generation)

(in-package #:clgrpc.client)

;;; Stub Base Class

(defclass grpc-stub ()
  ((channel
    :initarg :channel
    :reader stub-channel
    :documentation "gRPC channel for making calls"))
  (:documentation "Base class for all gRPC client stubs"))

;;; Helper: Auto-serializing call wrapper

(defun call-unary-auto (channel service-name method-name request-class response-class request-obj
                        &key timeout metadata)
  "Make a unary RPC call with automatic serialization/deserialization.

   Args:
     channel: gRPC channel
     service-name: Service name string (e.g., \"helloworld.Greeter\")
     method-name: Method name string (e.g., \"SayHello\")
     request-class: Request message class (symbol)
     response-class: Response message class (symbol)
     request-obj: Request object to send
     timeout: Optional timeout in milliseconds
     metadata: Optional metadata alist

   Returns:
     (values response-object status status-message metadata)"
  (let ((request-bytes (clgrpc.grpc:proto-serialize request-obj)))
    (multiple-value-bind (response-bytes status status-message response-metadata)
        (call-unary channel service-name method-name request-bytes
                    :timeout timeout
                    :metadata metadata)
      (if (= status clgrpc.grpc:+grpc-status-ok+)
          (values (clgrpc.grpc:proto-deserialize response-class response-bytes)
                  status
                  status-message
                  response-metadata)
          (values nil status status-message response-metadata)))))

;;; Stub Generation from CLOS Service Definition

(defmacro defstub (stub-name service-class &key (package *package*))
  "Generate a client stub from a CLOS service definition.

   Args:
     stub-name: Name for the stub class (e.g., greeter-stub)
     service-class: Service class to introspect (e.g., greeter-service)
     package: Package for generated code (defaults to current package)

   Example:
     ;; Server has:
     (defclass greeter-service (grpc-service) ...)
     (defgrpc-method say-hello ((service greeter-service) ...) ...)

     ;; Client generates stub:
     (defstub greeter-stub greeter-service)

     ;; Use it:
     (let ((stub (make-instance 'greeter-stub :channel my-channel)))
       (say-hello stub (make-hello-request :name \"World\")))"

  (declare (ignore package))  ; TODO: Use for symbol resolution

  (let* ((service-meta (find-class service-class))
         (service-name (clgrpc.grpc:get-service-name service-meta))
         (methods (clgrpc.grpc:list-grpc-methods service-meta))
         (method-defs nil))

    ;; Generate method definitions
    (dolist (method-info methods)
      (let* ((server-lisp-name (clgrpc.grpc:grpc-method-info-lisp-name method-info))
             ;; Create client-side method name in the current package, not the server's package
             (client-lisp-name (intern (symbol-name server-lisp-name) package))
             (grpc-name (clgrpc.grpc:grpc-method-info-grpc-name method-info))
             (rpc-type (clgrpc.grpc:grpc-method-info-rpc-type method-info))
             (request-type (clgrpc.grpc:grpc-method-info-request-type method-info))
             (response-type (clgrpc.grpc:grpc-method-info-response-type method-info))
             (doc (format nil "Call ~A RPC method" grpc-name)))

        (case rpc-type
          (:unary
           (push
            `(progn
               ;; Define the generic function if it doesn't exist
               (unless (fboundp ',client-lisp-name)
                 (defgeneric ,client-lisp-name (stub request &key timeout metadata)
                   ,@(when doc `((:documentation ,doc)))))

               ;; Define the method
               (defmethod ,client-lisp-name ((stub ,stub-name) request &key timeout metadata)
                 ,@(when doc `(,doc))
                 (multiple-value-bind (response status status-message response-metadata)
                     (call-unary-auto (stub-channel stub)
                                     ,service-name
                                     ,grpc-name
                                     ',request-type
                                     ',response-type
                                     request
                                     :timeout timeout
                                     :metadata metadata)
                   (if (= status clgrpc.grpc:+grpc-status-ok+)
                       (values response response-metadata)
                       (error 'clgrpc.grpc:grpc-error
                              :status-code status
                              :message (or status-message "RPC failed"))))))
            method-defs))

          ;; TODO: Add streaming stub methods
          ((:server-streaming :client-streaming :bidirectional)
           (format *error-output* "Warning: Stub generation for ~A not yet implemented~%"
                   rpc-type)))))

    ;; Generate the stub class and methods
    `(progn
       ;; Define the stub class
       (defclass ,stub-name (grpc-stub)
         ()
         (:documentation ,(format nil "Client stub for ~A service" service-name)))

       ;; Define all methods
       ,@(nreverse method-defs)

       ;; Return the stub class name
       ',stub-name)))

;;; Stub Generation from .proto File

;; TODO: Enable once parse-proto-file is exported from clgrpc.grpc
#+nil
(defun make-stub-from-proto-file (proto-file &key (package *package*) output-file)
  "Generate client stub code from a .proto file.

   Args:
     proto-file: Path to .proto file
     package: Package for generated code
     output-file: Optional file to write generated code to

   Returns:
     Generated code as a list of forms (if no output-file)
     or writes to output-file and returns the filename

   Example:
     (make-stub-from-proto-file \"helloworld.proto\"
                                :output-file \"helloworld-stub.lisp\")"

  ;; Parse the .proto file
  (let ((parsed (clgrpc.grpc:parse-proto-file proto-file)))

    ;; Extract service definitions
    (let ((services (getf parsed :services))
          (generated-code nil))

      ;; Generate code for each service
      (dolist (service services)
        (let* ((service-name (getf service :name))
               (package-name (getf parsed :package))
               (full-service-name (if package-name
                                      (format nil "~A.~A" package-name service-name)
                                      service-name))
               (stub-class-name (intern (format nil "~A-STUB"
                                               (string-upcase service-name))
                                       package))
               (methods (getf service :methods)))

          ;; Generate stub class
          (push `(defclass ,stub-class-name (grpc-stub)
                   ()
                   (:documentation ,(format nil "Client stub for ~A service" full-service-name)))
                generated-code)

          ;; Generate methods
          (dolist (method methods)
            (let* ((method-name (getf method :name))
                   (lisp-method-name (intern (string-upcase
                                             (camel-to-kebab-case method-name))
                                            package))
                   (request-type (intern (string-upcase (getf method :input-type)) package))
                   (response-type (intern (string-upcase (getf method :output-type)) package))
                   (rpc-type (or (getf method :stream-type) :unary)))

              (case rpc-type
                (:unary
                 (push
                  `(defmethod ,lisp-method-name ((stub ,stub-class-name) request
                                                &key timeout metadata)
                     ,(format nil "Call ~A RPC method" method-name)
                     (multiple-value-bind (response status status-message response-metadata)
                         (call-unary-auto (stub-channel stub)
                                         ,full-service-name
                                         ,method-name
                                         ',request-type
                                         ',response-type
                                         request
                                         :timeout timeout
                                         :metadata metadata)
                       (if (= status clgrpc.grpc:+grpc-status-ok+)
                           (values response response-metadata)
                           (error 'clgrpc.grpc:grpc-error
                                  :status-code status
                                  :message (or status-message "RPC failed")))))
                  generated-code))

                ;; TODO: Streaming methods
                (t
                 (format *error-output* "Warning: Stub generation for ~A not yet implemented~%"
                         rpc-type)))))))

      ;; Write to file or return code
      (setf generated-code (nreverse generated-code))
      (if output-file
          (with-open-file (out output-file
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
            (format out ";;;; Auto-generated client stub from ~A~%~%" proto-file)
            (format out "(in-package ~S)~%~%" (package-name package))
            (dolist (form generated-code)
              (format out "~S~%~%" form))
            (format t "Generated stub code written to ~A~%" output-file)
            output-file)
          generated-code))))

;;; Helper for CamelCase to kebab-case conversion

(defun camel-to-kebab-case (string)
  "Convert CamelCase to kebab-case.

   Examples:
     SayHello → say-hello
     GetFeature → get-feature"
  (with-output-to-string (out)
    (loop for i from 0 below (length string)
          for char = (char string i)
          for prev-char = (when (plusp i) (char string (1- i)))
          do (cond
               ;; Insert hyphen before uppercase if previous was lowercase
               ((and (upper-case-p char)
                     prev-char
                     (lower-case-p prev-char))
                (write-char #\- out)
                (write-char (char-downcase char) out))
               ;; Just downcase
               (t
                (write-char (char-downcase char) out))))))
