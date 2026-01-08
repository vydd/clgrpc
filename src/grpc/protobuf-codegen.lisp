;;;; protobuf-codegen.lisp - Parse .proto files and generate Lisp code
;;;;
;;;; Parses proto3 syntax and generates encoder/decoder functions

(in-package #:clgrpc.grpc)

;;; Proto3 parser
;;;
;;; Supports:
;;; - message definitions
;;; - field types (all protobuf types)
;;; - repeated fields
;;; - nested messages
;;; - comments

(defstruct proto-field
  name
  type
  number
  repeated)

(defstruct proto-message
  name
  fields)

(defun tokenize-proto (string)
  "Tokenize a .proto file into words."
  (let ((tokens nil)
        (current-token (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for char across string
          do (cond
               ;; Whitespace - end current token
               ((member char '(#\Space #\Tab #\Newline #\Return))
                (when (> (length current-token) 0)
                  (push (copy-seq current-token) tokens)
                  (setf (fill-pointer current-token) 0)))

               ;; Special characters - treat as separate tokens
               ((member char '(#\{ #\} #\; #\= #\< #\> #\( #\) #\[ #\]))
                (when (> (length current-token) 0)
                  (push (copy-seq current-token) tokens)
                  (setf (fill-pointer current-token) 0))
                (push (string char) tokens))

               ;; Regular character - add to current token
               (t
                (vector-push-extend char current-token))))

    ;; Push final token if any
    (when (> (length current-token) 0)
      (push (copy-seq current-token) tokens))

    (nreverse tokens)))

(defun strip-comments (string)
  "Remove // and /* */ style comments from proto file."
  (with-output-to-string (out)
    (let ((i 0)
          (len (length string)))
      (loop while (< i len)
            do (cond
                 ;; Line comment
                 ((and (< (1+ i) len)
                       (char= (char string i) #\/)
                       (char= (char string (1+ i)) #\/))
                  ;; Skip until end of line
                  (loop while (and (< i len) (char/= (char string i) #\Newline))
                        do (incf i)))

                 ;; Block comment
                 ((and (< (1+ i) len)
                       (char= (char string i) #\/)
                       (char= (char string (1+ i)) #\*))
                  ;; Skip until */
                  (incf i 2)
                  (loop while (< (1+ i) len)
                        do (when (and (char= (char string i) #\*)
                                      (char= (char string (1+ i)) #\/))
                             (incf i 2)
                             (return))
                           (incf i)))

                 ;; Regular character
                 (t
                  (write-char (char string i) out)
                  (incf i)))))))

(defun parse-proto-field (tokens)
  "Parse a field declaration. Returns (field . remaining-tokens)."
  (let ((repeated nil)
        (field-type nil)
        (field-name nil)
        (field-number nil))

    ;; Check for 'repeated'
    (when (string= (first tokens) "repeated")
      (setf repeated t)
      (pop tokens))

    ;; Get type
    (setf field-type (pop tokens))

    ;; Get name
    (setf field-name (pop tokens))

    ;; Expect '='
    (unless (string= (pop tokens) "=")
      (error "Expected '=' in field declaration"))

    ;; Get field number
    (setf field-number (parse-integer (pop tokens)))

    ;; Expect ';'
    (unless (string= (pop tokens) ";")
      (error "Expected ';' at end of field"))

    (values
     (make-proto-field
      :name field-name
      :type field-type
      :number field-number
      :repeated repeated)
     tokens)))

(defun parse-proto-message (tokens)
  "Parse a message definition. Returns (message . remaining-tokens)."
  (let ((message-name nil)
        (fields nil))

    ;; Expect 'message'
    (unless (string= (pop tokens) "message")
      (error "Expected 'message' keyword"))

    ;; Get message name
    (setf message-name (pop tokens))

    ;; Expect '{'
    (unless (string= (pop tokens) "{")
      (error "Expected '{' after message name"))

    ;; Parse fields until '}'
    (loop while (and tokens (not (string= (first tokens) "}")))
          do (multiple-value-bind (field remaining)
                 (parse-proto-field tokens)
               (push field fields)
               (setf tokens remaining)))

    ;; Expect '}'
    (unless (string= (pop tokens) "}")
      (error "Expected '}' at end of message"))

    (values
     (make-proto-message
      :name message-name
      :fields (nreverse fields))
     tokens)))

(defun parse-proto-file (proto-string)
  "Parse a .proto file and return list of message definitions."
  (let* ((cleaned (strip-comments proto-string))
         (tokens (tokenize-proto cleaned))
         (messages nil))

    ;; Skip syntax declaration
    (when (and tokens (string= (first tokens) "syntax"))
      (loop while (and tokens (not (string= (first tokens) ";")))
            do (pop tokens))
      (pop tokens)) ; skip the ';'

    ;; Skip package declaration
    (when (and tokens (string= (first tokens) "package"))
      (loop while (and tokens (not (string= (first tokens) ";")))
            do (pop tokens))
      (pop tokens)) ; skip the ';'

    ;; Parse messages
    (loop while tokens
          do (when (string= (first tokens) "message")
               (multiple-value-bind (message remaining)
                   (parse-proto-message tokens)
                 (push message messages)
                 (setf tokens remaining)))
             (when (and tokens (not (string= (first tokens) "message")))
               (pop tokens)))

    (nreverse messages)))

;;; Code generation

(defun proto-type-to-encoder (type-name)
  "Map proto type to encoder function name."
  (cond
    ((string= type-name "string") "encode-string-field")
    ((string= type-name "bytes") "encode-bytes-field")
    ((string= type-name "int32") "encode-int32-field")
    ((string= type-name "int64") "encode-int64-field")
    ((string= type-name "uint32") "encode-uint32-field")
    ((string= type-name "uint64") "encode-uint64-field")
    ((string= type-name "sint32") "encode-sint32-field")
    ((string= type-name "sint64") "encode-sint64-field")
    ((string= type-name "fixed32") "encode-fixed32-field")
    ((string= type-name "fixed64") "encode-fixed64-field")
    ((string= type-name "sfixed32") "encode-sfixed32-field")
    ((string= type-name "sfixed64") "encode-sfixed64-field")
    ((string= type-name "bool") "encode-bool-field")
    ((string= type-name "float") "encode-float-field")
    ((string= type-name "double") "encode-double-field")
    (t
     ;; Assume it's a message type
     (format nil "encode-~A" (string-downcase type-name)))))

(defun proto-type-to-wire-type (type-name)
  "Map proto type to wire type constant."
  (cond
    ((member type-name '("string" "bytes") :test #'string=)
     "+wire-type-length-delimited+")
    ((member type-name '("int32" "int64" "uint32" "uint64" "sint32" "sint64" "bool") :test #'string=)
     "+wire-type-varint+")
    ((member type-name '("fixed64" "sfixed64" "double") :test #'string=)
     "+wire-type-64bit+")
    ((member type-name '("fixed32" "sfixed32" "float") :test #'string=)
     "+wire-type-32bit+")
    (t
     ;; Message type
     "+wire-type-length-delimited+")))

(defun proto-type-to-decoder (type-name)
  "Map proto type to decoder function name."
  (cond
    ((string= type-name "string") "decode-string-field")
    ((string= type-name "bytes") "decode-bytes-field")
    ((member type-name '("int32" "int64" "uint32" "uint64" "sint32" "sint64" "bool") :test #'string=)
     "decode-varint")
    ((string= type-name "fixed32") "decode-fixed32")
    ((string= type-name "fixed64") "decode-fixed64")
    ((string= type-name "float") "pb-decode-float")
    ((string= type-name "double") "pb-decode-double")
    (t
     ;; Assume it's a message type
     (format nil "decode-~A" (string-downcase type-name)))))

(defun generate-encoder (message)
  "Generate Lisp encoder function for a message."
  (with-output-to-string (out)
    (let* ((msg-name (proto-message-name message))
           (fields (proto-message-fields message))
           (function-name (format nil "encode-~A" (string-downcase msg-name)))
           (param-names (mapcar (lambda (f)
                                 (intern (string-upcase (proto-field-name f))))
                               fields)))

      ;; Function definition
      (format out "(defun ~A (~{~A~^ ~})~%" function-name param-names)
      (format out "  \"Encode ~A message.\"~%" msg-name)
      (format out "  (encode-message~%")
      (format out "   (list~%")

      ;; Generate field encoders
      (loop for field in fields
            for param in param-names
            do (let* ((field-type (proto-field-type field))
                      (is-message (not (member field-type
                                              '("string" "bytes" "int32" "int64" "uint32" "uint64"
                                                "sint32" "sint64" "fixed32" "fixed64" "sfixed32" "sfixed64"
                                                "bool" "float" "double")
                                              :test #'string=))))
                 (cond
                   ;; Repeated field
                   ((proto-field-repeated field)
                    (format out "    (encode-repeated-field ~D ~A #'~A)~%"
                            (proto-field-number field)
                            param
                            (proto-type-to-encoder field-type)))
                   ;; Embedded message
                   (is-message
                    (format out "    (encode-message-field ~D (~A ~A))~%"
                            (proto-field-number field)
                            (proto-type-to-encoder field-type)
                            param))
                   ;; Primitive type
                   (t
                    (format out "    (~A ~D ~A)~%"
                            (proto-type-to-encoder field-type)
                            (proto-field-number field)
                            param)))))

      (format out "   )))~%"))))

(defun generate-decoder (message)
  "Generate Lisp decoder function for a message."
  (with-output-to-string (out)
    (let* ((msg-name (proto-message-name message))
           (fields (proto-message-fields message))
           (function-name (format nil "decode-~A" (string-downcase msg-name))))

      ;; Function definition
      (format out "(defun ~A (bytes)~%" function-name)
      (format out "  \"Decode ~A message.\"~%" msg-name)

      ;; Initialize result variables
      (format out "  (let (")
      (loop for field in fields
            do (let ((var-name (intern (string-upcase (proto-field-name field)))))
                 (if (proto-field-repeated field)
                     (format out "(~A nil) " var-name)
                     (format out "(~A ~A) "
                             var-name
                             (cond
                               ((string= (proto-field-type field) "string") "\"\"")
                               ((string= (proto-field-type field) "bool") "nil")
                               ((member (proto-field-type field) '("float" "double") :test #'string=) "0.0")
                               (t "0"))))))
      (format out "(offset 0))~%")

      ;; Decode loop
      (format out "    (loop while (< offset (length bytes))~%")
      (format out "          do (multiple-value-bind (field-number wire-type new-offset)~%")
      (format out "                 (decode-field-tag bytes offset)~%")
      (format out "               (setf offset new-offset)~%")
      (format out "               (cond~%")

      ;; Generate field decoders
      (loop for field in fields
            do (let* ((field-num (proto-field-number field))
                      (field-type (proto-field-type field))
                      (wire-type (proto-type-to-wire-type field-type))
                      (decoder (proto-type-to-decoder field-type))
                      (var-name (intern (string-upcase (proto-field-name field))))
                      (is-message (not (member field-type
                                              '("string" "bytes" "int32" "int64" "uint32" "uint64"
                                                "sint32" "sint64" "fixed32" "fixed64" "sfixed32" "sfixed64"
                                                "bool" "float" "double")
                                              :test #'string=))))
                 (format out "                 ((and (= field-number ~D) (= wire-type ~A))~%"
                         field-num wire-type)
                 (if is-message
                     ;; Embedded message - decode bytes first, then message
                     (progn
                       (format out "                  (multiple-value-bind (message-bytes new-offset)~%")
                       (format out "                      (decode-message-field bytes offset)~%")
                       (format out "                    (setf ~A (~A message-bytes))~%" var-name decoder)
                       (format out "                    (setf offset new-offset)))~%"))
                     ;; Primitive type
                     (progn
                       (format out "                  (multiple-value-bind (value new-offset)~%")
                       (format out "                      (~A bytes offset)~%" decoder)
                       (if (proto-field-repeated field)
                           (format out "                    (push value ~A)~%" var-name)
                           (format out "                    (setf ~A value)~%" var-name))
                       (format out "                    (setf offset new-offset)))~%")))))

      ;; Unknown field handling
      (format out "                 (t~%")
      (format out "                  (setf offset (skip-field bytes offset wire-type))))))~%")

      ;; Return values
      (format out "    (values ")
      (loop for field in fields
            for i from 0
            do (let ((var-name (intern (string-upcase (proto-field-name field)))))
                 (if (proto-field-repeated field)
                     (format out "~A(reverse ~A)" (if (> i 0) " " "") var-name)
                     (format out "~A~A" (if (> i 0) " " "") var-name))))
      (format out ")))~%"))))

(defun generate-code-from-proto (proto-string)
  "Parse .proto file and generate encoder/decoder functions."
  (let ((messages (parse-proto-file proto-string)))
    (with-output-to-string (out)
      (format out ";;;; Auto-generated from .proto file~%~%")
      (loop for message in messages
            do (format out "~%;;; ~A message~%~%" (proto-message-name message))
               (format out "~A~%" (generate-encoder message))
               (format out "~A~%" (generate-decoder message))))))

(defun compile-proto-file (proto-file-path &optional (output-file-path nil))
  "Compile a .proto file to Lisp code."
  (let* ((proto-string (with-open-file (in proto-file-path)
                         (let ((str (make-string (file-length in))))
                           (read-sequence str in)
                           str)))
         (generated-code (generate-code-from-proto proto-string)))

    (if output-file-path
        ;; Write to file
        (with-open-file (out output-file-path
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (write-string generated-code out))
        ;; Return string
        generated-code)))
