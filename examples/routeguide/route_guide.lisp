;;;; Auto-generated from .proto file


;;; Point message

(defun encode-point (LATITUDE LONGITUDE)
  "Encode Point message."
  (encode-message
   (list
    (encode-int32-field 1 LATITUDE)
    (encode-int32-field 2 LONGITUDE)
   )))

(defun decode-point (bytes)
  "Decode Point message."
  (let ((LATITUDE 0) (LONGITUDE 0) (offset 0))
    (loop while (< offset (length bytes))
          do (multiple-value-bind (field-number wire-type new-offset)
                 (decode-field-tag bytes offset)
               (setf offset new-offset)
               (cond
                 ((and (= field-number 1) (= wire-type +wire-type-varint+))
                  (multiple-value-bind (value new-offset)
                      (decode-varint bytes offset)
                    (setf LATITUDE value)
                    (setf offset new-offset)))
                 ((and (= field-number 2) (= wire-type +wire-type-varint+))
                  (multiple-value-bind (value new-offset)
                      (decode-varint bytes offset)
                    (setf LONGITUDE value)
                    (setf offset new-offset)))
                 (t
                  (setf offset (skip-field bytes offset wire-type))))))
    (values LATITUDE LONGITUDE)))


;;; Rectangle message

(defun encode-rectangle (LO HI)
  "Encode Rectangle message."
  (encode-message
   (list
    (encode-message-field 1 (encode-point LO))
    (encode-message-field 2 (encode-point HI))
   )))

(defun decode-rectangle (bytes)
  "Decode Rectangle message."
  (let ((LO 0) (HI 0) (offset 0))
    (loop while (< offset (length bytes))
          do (multiple-value-bind (field-number wire-type new-offset)
                 (decode-field-tag bytes offset)
               (setf offset new-offset)
               (cond
                 ((and (= field-number 1) (= wire-type +wire-type-length-delimited+))
                  (multiple-value-bind (message-bytes new-offset)
                      (decode-message-field bytes offset)
                    (setf LO (decode-point message-bytes))
                    (setf offset new-offset)))
                 ((and (= field-number 2) (= wire-type +wire-type-length-delimited+))
                  (multiple-value-bind (message-bytes new-offset)
                      (decode-message-field bytes offset)
                    (setf HI (decode-point message-bytes))
                    (setf offset new-offset)))
                 (t
                  (setf offset (skip-field bytes offset wire-type))))))
    (values LO HI)))


;;; Feature message

(defun encode-feature (NAME LOCATION)
  "Encode Feature message."
  (encode-message
   (list
    (encode-string-field 1 NAME)
    (encode-message-field 2 (encode-point LOCATION))
   )))

(defun decode-feature (bytes)
  "Decode Feature message."
  (let ((NAME "") (LOCATION 0) (offset 0))
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
                 ((and (= field-number 2) (= wire-type +wire-type-length-delimited+))
                  (multiple-value-bind (message-bytes new-offset)
                      (decode-message-field bytes offset)
                    (setf LOCATION (decode-point message-bytes))
                    (setf offset new-offset)))
                 (t
                  (setf offset (skip-field bytes offset wire-type))))))
    (values NAME LOCATION)))


;;; RouteNote message

(defun encode-routenote (LOCATION MESSAGE)
  "Encode RouteNote message."
  (encode-message
   (list
    (encode-message-field 1 (encode-point LOCATION))
    (encode-string-field 2 MESSAGE)
   )))

(defun decode-routenote (bytes)
  "Decode RouteNote message."
  (let ((LOCATION 0) (MESSAGE "") (offset 0))
    (loop while (< offset (length bytes))
          do (multiple-value-bind (field-number wire-type new-offset)
                 (decode-field-tag bytes offset)
               (setf offset new-offset)
               (cond
                 ((and (= field-number 1) (= wire-type +wire-type-length-delimited+))
                  (multiple-value-bind (message-bytes new-offset)
                      (decode-message-field bytes offset)
                    (setf LOCATION (decode-point message-bytes))
                    (setf offset new-offset)))
                 ((and (= field-number 2) (= wire-type +wire-type-length-delimited+))
                  (multiple-value-bind (value new-offset)
                      (decode-string-field bytes offset)
                    (setf MESSAGE value)
                    (setf offset new-offset)))
                 (t
                  (setf offset (skip-field bytes offset wire-type))))))
    (values LOCATION MESSAGE)))


;;; RouteSummary message

(defun encode-routesummary (POINT_COUNT FEATURE_COUNT DISTANCE ELAPSED_TIME)
  "Encode RouteSummary message."
  (encode-message
   (list
    (encode-int32-field 1 POINT_COUNT)
    (encode-int32-field 2 FEATURE_COUNT)
    (encode-int32-field 3 DISTANCE)
    (encode-int32-field 4 ELAPSED_TIME)
   )))

(defun decode-routesummary (bytes)
  "Decode RouteSummary message."
  (let ((POINT_COUNT 0) (FEATURE_COUNT 0) (DISTANCE 0) (ELAPSED_TIME 0) (offset 0))
    (loop while (< offset (length bytes))
          do (multiple-value-bind (field-number wire-type new-offset)
                 (decode-field-tag bytes offset)
               (setf offset new-offset)
               (cond
                 ((and (= field-number 1) (= wire-type +wire-type-varint+))
                  (multiple-value-bind (value new-offset)
                      (decode-varint bytes offset)
                    (setf POINT_COUNT value)
                    (setf offset new-offset)))
                 ((and (= field-number 2) (= wire-type +wire-type-varint+))
                  (multiple-value-bind (value new-offset)
                      (decode-varint bytes offset)
                    (setf FEATURE_COUNT value)
                    (setf offset new-offset)))
                 ((and (= field-number 3) (= wire-type +wire-type-varint+))
                  (multiple-value-bind (value new-offset)
                      (decode-varint bytes offset)
                    (setf DISTANCE value)
                    (setf offset new-offset)))
                 ((and (= field-number 4) (= wire-type +wire-type-varint+))
                  (multiple-value-bind (value new-offset)
                      (decode-varint bytes offset)
                    (setf ELAPSED_TIME value)
                    (setf offset new-offset)))
                 (t
                  (setf offset (skip-field bytes offset wire-type))))))
    (values POINT_COUNT FEATURE_COUNT DISTANCE ELAPSED_TIME)))

