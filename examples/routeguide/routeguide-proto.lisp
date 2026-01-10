;;;; routeguide-proto.lisp - RouteGuide protocol buffer encoders/decoders
;;;;
;;;; Hand-written protobuf code for RouteGuide messages

(defpackage #:routeguide
  (:use #:cl #:clgrpc.utils #:clgrpc.grpc)
  (:export
   ;; Data structures
   #:point #:make-point #:point-latitude #:point-longitude
   #:rectangle #:make-rectangle #:rectangle-lo #:rectangle-hi
   #:feature #:make-feature #:feature-name #:feature-location
   #:route-note #:make-route-note #:route-note-location #:route-note-message
   #:route-summary #:make-route-summary
   #:route-summary-point-count #:route-summary-feature-count
   #:route-summary-distance #:route-summary-elapsed-time
   ;; Encoders/Decoders
   #:encode-point #:decode-point
   #:encode-rectangle #:decode-rectangle
   #:encode-feature #:decode-feature
   #:encode-route-note #:decode-route-note
   #:encode-route-summary #:decode-route-summary))

(in-package #:routeguide)

;;; Data structures

(defstruct point
  "A point in E7 representation (degrees * 10^7)"
  (latitude 0 :type (signed-byte 32))
  (longitude 0 :type (signed-byte 32)))

(defstruct rectangle
  "A latitude-longitude rectangle"
  (lo nil :type (or null point))
  (hi nil :type (or null point)))

(defstruct feature
  "A named feature at a location"
  (name "" :type string)
  (location nil :type (or null point)))

(defstruct route-note
  "A message sent at a location"
  (location nil :type (or null point))
  (message "" :type string))

(defstruct route-summary
  "Summary of a recorded route"
  (point-count 0 :type (signed-byte 32))
  (feature-count 0 :type (signed-byte 32))
  (distance 0 :type (signed-byte 32))
  (elapsed-time 0 :type (signed-byte 32)))

;;; Point encoding/decoding

(defun encode-point (point)
  "Encode Point message to bytes."
  (clgrpc.grpc:encode-message
   (list
    (clgrpc.grpc:encode-int32-field 1 (point-latitude point))
    (clgrpc.grpc:encode-int32-field 2 (point-longitude point)))))

(defun decode-point (bytes)
  "Decode Point message from bytes."
  (let ((latitude 0)
        (longitude 0)
        (offset 0))
    (loop while (< offset (length bytes))
          do (multiple-value-bind (field-number wire-type new-offset)
                 (clgrpc.grpc:decode-field-tag bytes offset)
               (setf offset new-offset)
               (cond
                 ((and (= field-number 1) (= wire-type clgrpc.grpc:+wire-type-varint+))
                  (multiple-value-bind (value new-offset)
                      (clgrpc.grpc:decode-int32 bytes offset)
                    (setf latitude value)
                    (setf offset new-offset)))
                 ((and (= field-number 2) (= wire-type clgrpc.grpc:+wire-type-varint+))
                  (multiple-value-bind (value new-offset)
                      (clgrpc.grpc:decode-int32 bytes offset)
                    (setf longitude value)
                    (setf offset new-offset)))
                 (t
                  (setf offset (clgrpc.grpc:skip-field bytes offset wire-type))))))
    (make-point :latitude latitude :longitude longitude)))

;;; Rectangle encoding/decoding

(defun encode-rectangle (rectangle)
  "Encode Rectangle message to bytes."
  (clgrpc.grpc:encode-message
   (list
    (when (rectangle-lo rectangle)
      (clgrpc.grpc:encode-message-field 1 (encode-point (rectangle-lo rectangle))))
    (when (rectangle-hi rectangle)
      (clgrpc.grpc:encode-message-field 2 (encode-point (rectangle-hi rectangle)))))))

(defun decode-rectangle (bytes)
  "Decode Rectangle message from bytes."
  (let ((lo nil)
        (hi nil)
        (offset 0))
    (loop while (< offset (length bytes))
          do (multiple-value-bind (field-number wire-type new-offset)
                 (clgrpc.grpc:decode-field-tag bytes offset)
               (setf offset new-offset)
               (cond
                 ((and (= field-number 1) (= wire-type clgrpc.grpc:+wire-type-length-delimited+))
                  (multiple-value-bind (msg-bytes new-offset)
                      (clgrpc.grpc:decode-length-delimited bytes offset)
                    (setf lo (decode-point msg-bytes))
                    (setf offset new-offset)))
                 ((and (= field-number 2) (= wire-type clgrpc.grpc:+wire-type-length-delimited+))
                  (multiple-value-bind (msg-bytes new-offset)
                      (clgrpc.grpc:decode-length-delimited bytes offset)
                    (setf hi (decode-point msg-bytes))
                    (setf offset new-offset)))
                 (t
                  (setf offset (clgrpc.grpc:skip-field bytes offset wire-type))))))
    (make-rectangle :lo lo :hi hi)))

;;; Feature encoding/decoding

(defun encode-feature (feature)
  "Encode Feature message to bytes."
  (clgrpc.grpc:encode-message
   (list
    (when (> (length (feature-name feature)) 0)
      (clgrpc.grpc:encode-string-field 1 (feature-name feature)))
    (when (feature-location feature)
      (clgrpc.grpc:encode-message-field 2 (encode-point (feature-location feature)))))))

(defun decode-feature (bytes)
  "Decode Feature message from bytes."
  (let ((name "")
        (location nil)
        (offset 0))
    (loop while (< offset (length bytes))
          do (multiple-value-bind (field-number wire-type new-offset)
                 (clgrpc.grpc:decode-field-tag bytes offset)
               (setf offset new-offset)
               (cond
                 ((and (= field-number 1) (= wire-type clgrpc.grpc:+wire-type-length-delimited+))
                  (multiple-value-bind (str-bytes new-offset)
                      (clgrpc.grpc:decode-length-delimited bytes offset)
                    (setf name (babel:octets-to-string str-bytes :encoding :utf-8))
                    (setf offset new-offset)))
                 ((and (= field-number 2) (= wire-type clgrpc.grpc:+wire-type-length-delimited+))
                  (multiple-value-bind (msg-bytes new-offset)
                      (clgrpc.grpc:decode-length-delimited bytes offset)
                    (setf location (decode-point msg-bytes))
                    (setf offset new-offset)))
                 (t
                  (setf offset (clgrpc.grpc:skip-field bytes offset wire-type))))))
    (make-feature :name name :location location)))

;;; RouteNote encoding/decoding

(defun encode-route-note (note)
  "Encode RouteNote message to bytes."
  (clgrpc.grpc:encode-message
   (list
    (when (route-note-location note)
      (clgrpc.grpc:encode-message-field 1 (encode-point (route-note-location note))))
    (when (> (length (route-note-message note)) 0)
      (clgrpc.grpc:encode-string-field 2 (route-note-message note))))))

(defun decode-route-note (bytes)
  "Decode RouteNote message from bytes."
  (let ((location nil)
        (message "")
        (offset 0))
    (loop while (< offset (length bytes))
          do (multiple-value-bind (field-number wire-type new-offset)
                 (clgrpc.grpc:decode-field-tag bytes offset)
               (setf offset new-offset)
               (cond
                 ((and (= field-number 1) (= wire-type clgrpc.grpc:+wire-type-length-delimited+))
                  (multiple-value-bind (msg-bytes new-offset)
                      (clgrpc.grpc:decode-length-delimited bytes offset)
                    (setf location (decode-point msg-bytes))
                    (setf offset new-offset)))
                 ((and (= field-number 2) (= wire-type clgrpc.grpc:+wire-type-length-delimited+))
                  (multiple-value-bind (str-bytes new-offset)
                      (clgrpc.grpc:decode-length-delimited bytes offset)
                    (setf message (babel:octets-to-string str-bytes :encoding :utf-8))
                    (setf offset new-offset)))
                 (t
                  (setf offset (clgrpc.grpc:skip-field bytes offset wire-type))))))
    (make-route-note :location location :message message)))

;;; RouteSummary encoding/decoding

(defun encode-route-summary (summary)
  "Encode RouteSummary message to bytes."
  (clgrpc.grpc:encode-message
   (list
    (clgrpc.grpc:encode-int32-field 1 (route-summary-point-count summary))
    (clgrpc.grpc:encode-int32-field 2 (route-summary-feature-count summary))
    (clgrpc.grpc:encode-int32-field 3 (route-summary-distance summary))
    (clgrpc.grpc:encode-int32-field 4 (route-summary-elapsed-time summary)))))

(defun decode-route-summary (bytes)
  "Decode RouteSummary message from bytes."
  (let ((point-count 0)
        (feature-count 0)
        (distance 0)
        (elapsed-time 0)
        (offset 0))
    (loop while (< offset (length bytes))
          do (multiple-value-bind (field-number wire-type new-offset)
                 (clgrpc.grpc:decode-field-tag bytes offset)
               (setf offset new-offset)
               (cond
                 ((and (= field-number 1) (= wire-type clgrpc.grpc:+wire-type-varint+))
                  (multiple-value-bind (value new-offset)
                      (clgrpc.grpc:decode-int32 bytes offset)
                    (setf point-count value)
                    (setf offset new-offset)))
                 ((and (= field-number 2) (= wire-type clgrpc.grpc:+wire-type-varint+))
                  (multiple-value-bind (value new-offset)
                      (clgrpc.grpc:decode-int32 bytes offset)
                    (setf feature-count value)
                    (setf offset new-offset)))
                 ((and (= field-number 3) (= wire-type clgrpc.grpc:+wire-type-varint+))
                  (multiple-value-bind (value new-offset)
                      (clgrpc.grpc:decode-int32 bytes offset)
                    (setf distance value)
                    (setf offset new-offset)))
                 ((and (= field-number 4) (= wire-type clgrpc.grpc:+wire-type-varint+))
                  (multiple-value-bind (value new-offset)
                      (clgrpc.grpc:decode-int32 bytes offset)
                    (setf elapsed-time value)
                    (setf offset new-offset)))
                 (t
                  (setf offset (clgrpc.grpc:skip-field bytes offset wire-type))))))
    (make-route-summary :point-count point-count
                        :feature-count feature-count
                        :distance distance
                        :elapsed-time elapsed-time)))
