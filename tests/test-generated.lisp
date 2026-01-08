(in-package #:clgrpc.grpc)

;;;; Auto-generated from .proto file


;;; Person message

(defun encode-person (NAME ID EMAIL PHONES)
  "Encode Person message."
  (encode-message
   (list
    (encode-string-field 1 NAME)
    (encode-int32-field 2 ID)
    (encode-string-field 3 EMAIL)
    (encode-repeated-field 4 PHONES #'encode-string-field)
   )))

(defun decode-person (bytes)
  "Decode Person message."
  (let ((NAME "") (ID 0) (EMAIL "") (PHONES nil) (offset 0))
    (loop while (< offset (length bytes))
          do (multiple-value-bind (field-number wire-type new-offset)
                 (decode-field-tag bytes offset)
               (setf offset new-offset)
               (cond
                 ((and (= field-number 1) (= wire-type +wire-type-length-delimited+))
                  (multiple-value-bind (value new-offset)
                      (decode-string-field bytes offset)
                    (setf NAME value)
                    (setf offset new-offset)))
                 ((and (= field-number 2) (= wire-type +wire-type-varint+))
                  (multiple-value-bind (value new-offset)
                      (decode-varint bytes offset)
                    (setf ID value)
                    (setf offset new-offset)))
                 ((and (= field-number 3) (= wire-type +wire-type-length-delimited+))
                  (multiple-value-bind (value new-offset)
                      (decode-string-field bytes offset)
                    (setf EMAIL value)
                    (setf offset new-offset)))
                 ((and (= field-number 4) (= wire-type +wire-type-length-delimited+))
                  (multiple-value-bind (value new-offset)
                      (decode-string-field bytes offset)
                    (push value PHONES)
                    (setf offset new-offset)))
                 (t
                  (setf offset (skip-field bytes offset wire-type))))))
    (values NAME ID EMAIL (reverse PHONES))))


;;; Address message

(defun encode-address (STREET CITY ZIP_CODE)
  "Encode Address message."
  (encode-message
   (list
    (encode-string-field 1 STREET)
    (encode-string-field 2 CITY)
    (encode-int32-field 3 ZIP_CODE)
   )))

(defun decode-address (bytes)
  "Decode Address message."
  (let ((STREET "") (CITY "") (ZIP_CODE 0) (offset 0))
    (loop while (< offset (length bytes))
          do (multiple-value-bind (field-number wire-type new-offset)
                 (decode-field-tag bytes offset)
               (setf offset new-offset)
               (cond
                 ((and (= field-number 1) (= wire-type +wire-type-length-delimited+))
                  (multiple-value-bind (value new-offset)
                      (decode-string-field bytes offset)
                    (setf STREET value)
                    (setf offset new-offset)))
                 ((and (= field-number 2) (= wire-type +wire-type-length-delimited+))
                  (multiple-value-bind (value new-offset)
                      (decode-string-field bytes offset)
                    (setf CITY value)
                    (setf offset new-offset)))
                 ((and (= field-number 3) (= wire-type +wire-type-varint+))
                  (multiple-value-bind (value new-offset)
                      (decode-varint bytes offset)
                    (setf ZIP_CODE value)
                    (setf offset new-offset)))
                 (t
                  (setf offset (skip-field bytes offset wire-type))))))
    (values STREET CITY ZIP_CODE)))


;;; Employee message

(defun encode-employee (PERSON ADDRESS SALARY ACTIVE)
  "Encode Employee message."
  (encode-message
   (list
    (encode-message-field 1 (encode-person PERSON))
    (encode-message-field 2 (encode-address ADDRESS))
    (encode-double-field 3 SALARY)
    (encode-bool-field 4 ACTIVE)
   )))

(defun decode-employee (bytes)
  "Decode Employee message."
  (let ((PERSON 0) (ADDRESS 0) (SALARY 0.0) (ACTIVE nil) (offset 0))
    (loop while (< offset (length bytes))
          do (multiple-value-bind (field-number wire-type new-offset)
                 (decode-field-tag bytes offset)
               (setf offset new-offset)
               (cond
                 ((and (= field-number 1) (= wire-type +wire-type-length-delimited+))
                  (multiple-value-bind (message-bytes new-offset)
                      (decode-message-field bytes offset)
                    (setf PERSON (decode-person message-bytes))
                    (setf offset new-offset)))
                 ((and (= field-number 2) (= wire-type +wire-type-length-delimited+))
                  (multiple-value-bind (message-bytes new-offset)
                      (decode-message-field bytes offset)
                    (setf ADDRESS (decode-address message-bytes))
                    (setf offset new-offset)))
                 ((and (= field-number 3) (= wire-type +wire-type-64bit+))
                  (multiple-value-bind (value new-offset)
                      (pb-decode-double bytes offset)
                    (setf SALARY value)
                    (setf offset new-offset)))
                 ((and (= field-number 4) (= wire-type +wire-type-varint+))
                  (multiple-value-bind (value new-offset)
                      (decode-varint bytes offset)
                    (setf ACTIVE value)
                    (setf offset new-offset)))
                 (t
                  (setf offset (skip-field bytes offset wire-type))))))
    (values PERSON ADDRESS SALARY ACTIVE)))

