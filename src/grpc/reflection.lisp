;;;; reflection.lisp - gRPC Server Reflection Protocol
;;;;
;;;; Implements the grpc.reflection.v1alpha.ServerReflection service
;;;; allowing clients to discover available services and methods.
;;;;
;;;; Protocol: https://github.com/grpc/grpc/blob/master/doc/server-reflection.md

(in-package #:clgrpc.grpc)

;;; Protocol Buffer Encoding for Reflection Messages
;;;
;;; Simplified implementation supporting the most common operation: list_services

;;; Helper: Concatenate byte arrays

(defun concat-bytes (&rest arrays)
  "Concatenate multiple byte arrays into one."
  (let* ((total-length (reduce #'+ arrays :key #'length :initial-value 0))
         (result (make-byte-array total-length))
         (pos 0))
    (dolist (arr arrays)
      (replace result arr :start1 pos)
      (incf pos (length arr)))
    result))

;;; Message: ServerReflectionRequest
;;; Fields:
;;;   1: string host
;;;   7: string list_services (oneof message_request)

(defun encode-reflection-request-list-services (host)
  "Encode a ServerReflectionRequest with list_services.

   message ServerReflectionRequest {
     string host = 1;
     string list_services = 7;  // simplified - normally oneof
   }"
  (let ((parts nil))
    ;; Field 1: host (string, wire type 2 = length-delimited)
    (when (and host (> (length host) 0))
      (let ((host-bytes (babel:string-to-octets host :encoding :utf-8)))
        ;; Tag: (field << 3) | wire_type = (1 << 3) | 2 = 10 = 0x0A
        (push (encode-tag-and-string 1 host-bytes) parts)))

    ;; Field 7: list_services (string, wire type 2)
    ;; Tag: (7 << 3) | 2 = 58 = 0x3A
    (push (make-byte-array 2 :initial-contents '(#x3A #x00)) parts)  ; Tag + length 0

    (apply #'concat-bytes (nreverse parts))))

(defun encode-tag-and-string (field-number str-bytes)
  "Encode a string field: tag + length + bytes."
  (let ((tag (logior (ash field-number 3) 2)))  ; wire type 2
    (concat-bytes
     (encode-varint tag)
     (encode-varint (length str-bytes))
     str-bytes)))

(defun decode-reflection-request (bytes)
  "Decode a ServerReflectionRequest.

   Returns: (values host message-type)
   where message-type is :list-services, :file-by-filename, etc."
  (let ((host "")
        (message-type nil)
        (pos 0))
    (loop while (< pos (length bytes))
          do (multiple-value-bind (tag new-pos)
                 (decode-varint bytes pos)
               (setf pos new-pos)
               (let* ((field-number (ash tag -3))
                      (wire-type (logand tag #x07)))
                 (cond
                   ;; Field 1: host
                   ((= field-number 1)
                    (multiple-value-bind (len new-pos)
                        (decode-varint bytes pos)
                      (setf pos new-pos)
                      (let ((str-bytes (subseq bytes pos (+ pos len))))
                        (setf host (babel:octets-to-string str-bytes :encoding :utf-8))
                        (incf pos len))))

                   ;; Field 7: list_services
                   ((= field-number 7)
                    (multiple-value-bind (len new-pos)
                        (decode-varint bytes pos)
                      (setf pos (+ new-pos len))
                      (setf message-type :list-services)))

                   ;; Field 3: file_by_filename
                   ((= field-number 3)
                    (multiple-value-bind (len new-pos)
                        (decode-varint bytes pos)
                      (setf pos (+ new-pos len))
                      (setf message-type :file-by-filename)))

                   ;; Field 4: file_containing_symbol
                   ((= field-number 4)
                    (multiple-value-bind (len new-pos)
                        (decode-varint bytes pos)
                      (setf pos (+ new-pos len))
                      (setf message-type :file-containing-symbol)))

                   ;; Unknown field - skip based on wire type
                   (t
                    (setf pos (skip-field-at wire-type bytes pos)))))))
    (values host message-type)))

;;; Message: ServiceResponse
;;; Fields:
;;;   1: string name

(defun encode-service-response (service-name)
  "Encode a ServiceResponse message.

   message ServiceResponse {
     string name = 1;
   }"
  (let ((name-bytes (babel:string-to-octets service-name :encoding :utf-8)))
    (encode-tag-and-string 1 name-bytes)))

;;; Message: ListServiceResponse
;;; Fields:
;;;   1: repeated ServiceResponse service

(defun encode-list-services-response (service-names)
  "Encode a ListServiceResponse message.

   message ListServiceResponse {
     repeated ServiceResponse service = 1;
   }"
  (let ((parts nil))
    (dolist (name service-names)
      (let ((service-msg (encode-service-response name)))
        ;; Tag: (1 << 3) | 2 = 10 = 0x0A
        (push (concat-bytes
               (encode-varint 10)
               (encode-varint (length service-msg))
               service-msg)
              parts)))
    (if parts
        (apply #'concat-bytes (nreverse parts))
        (make-byte-array 0))))

;;; Message: ServerReflectionResponse
;;; Fields:
;;;   1: string valid_host
;;;   2: ServerReflectionRequest original_request
;;;   6: ListServiceResponse list_services_response (oneof message_response)

(defun encode-reflection-response-list-services (host original-request service-names)
  "Encode a ServerReflectionResponse with list_services_response.

   message ServerReflectionResponse {
     string valid_host = 1;
     ServerReflectionRequest original_request = 2;
     ListServiceResponse list_services_response = 6;  // simplified - normally oneof
   }"
  (let ((parts nil))
    ;; Field 1: valid_host
    (when (and host (> (length host) 0))
      (let ((host-bytes (babel:string-to-octets host :encoding :utf-8)))
        (push (encode-tag-and-string 1 host-bytes) parts)))

    ;; Field 2: original_request (embedded message)
    (when original-request
      ;; Tag: (2 << 3) | 2 = 18 = 0x12
      (push (concat-bytes
             (encode-varint 18)
             (encode-varint (length original-request))
             original-request)
            parts))

    ;; Field 6: list_services_response (embedded message)
    (let ((list-response (encode-list-services-response service-names)))
      ;; Tag: (6 << 3) | 2 = 50 = 0x32
      (push (concat-bytes
             (encode-varint 50)
             (encode-varint (length list-response))
             list-response)
            parts))

    (apply #'concat-bytes (nreverse parts))))

;;; Helper: Skip unknown fields

(defun skip-field-at (wire-type bytes pos)
  "Skip an unknown field based on wire type. Returns new position."
  (cond
    ;; Varint (wire type 0)
    ((= wire-type 0)
     (multiple-value-bind (value new-pos)
         (decode-varint bytes pos)
       (declare (ignore value))
       new-pos))

    ;; 64-bit (wire type 1)
    ((= wire-type 1)
     (+ pos 8))

    ;; Length-delimited (wire type 2)
    ((= wire-type 2)
     (multiple-value-bind (len new-pos)
         (decode-varint bytes pos)
       (+ new-pos len)))

    ;; 32-bit (wire type 5)
    ((= wire-type 5)
     (+ pos 4))

    ;; Unknown wire type
    (t
     (error "Unknown wire type: ~D" wire-type))))
