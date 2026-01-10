;;;; routeguide-proto.lisp - RouteGuide protocol buffer messages
;;;;
;;;; CLOS-based protobuf messages for RouteGuide

(defpackage #:routeguide
  (:use #:cl #:clgrpc.utils #:clgrpc.grpc)
  (:export
   ;; Data structures
   #:point
   #:make-point
   #:point-latitude
   #:point-longitude
   #:rectangle
   #:make-rectangle
   #:rectangle-lo
   #:rectangle-hi
   #:feature
   #:make-feature
   #:feature-name
   #:feature-location
   #:route-note
   #:make-route-note
   #:route-note-location
   #:route-note-message
   #:route-summary
   #:make-route-summary
   #:route-summary-point-count
   #:route-summary-feature-count
   #:route-summary-distance
   #:route-summary-elapsed-time))

(in-package #:routeguide)

;;; Message Definitions using proto-metaclass

(defclass point (proto-message)
  ((latitude
    :initarg :latitude
    :initform 0
    :accessor point-latitude
    :field 1
    :proto-type :int32
    :documentation "Latitude in E7 representation")
   (longitude
    :initarg :longitude
    :initform 0
    :accessor point-longitude
    :field 2
    :proto-type :int32
    :documentation "Longitude in E7 representation"))
  (:metaclass proto-metaclass)
  (:documentation "A point in E7 representation (degrees * 10^7)"))

(defun make-point (&key (latitude 0) (longitude 0))
  "Create a Point instance."
  (make-instance 'point :latitude latitude :longitude longitude))

(defclass rectangle (proto-message)
  ((lo
    :initarg :lo
    :initform nil
    :accessor rectangle-lo
    :field 1
    :proto-type :message
    :message-type point
    :documentation "One corner of the rectangle")
   (hi
    :initarg :hi
    :initform nil
    :accessor rectangle-hi
    :field 2
    :proto-type :message
    :message-type point
    :documentation "Other corner of the rectangle"))
  (:metaclass proto-metaclass)
  (:documentation "A latitude-longitude rectangle"))

(defun make-rectangle (&key lo hi)
  "Create a Rectangle instance."
  (make-instance 'rectangle :lo lo :hi hi))

(defclass feature (proto-message)
  ((name
    :initarg :name
    :initform ""
    :accessor feature-name
    :field 1
    :proto-type :string
    :documentation "Name of the feature")
   (location
    :initarg :location
    :initform nil
    :accessor feature-location
    :field 2
    :proto-type :message
    :message-type point
    :documentation "Location of the feature"))
  (:metaclass proto-metaclass)
  (:documentation "A named feature at a location"))

(defun make-feature (&key (name "") location)
  "Create a Feature instance."
  (make-instance 'feature :name name :location location))

(defclass route-note (proto-message)
  ((location
    :initarg :location
    :initform nil
    :accessor route-note-location
    :field 1
    :proto-type :message
    :message-type point
    :documentation "Location of the note")
   (message
    :initarg :message
    :initform ""
    :accessor route-note-message
    :field 2
    :proto-type :string
    :documentation "Message content"))
  (:metaclass proto-metaclass)
  (:documentation "A message sent at a location"))

(defun make-route-note (&key location (message ""))
  "Create a RouteNote instance."
  (make-instance 'route-note :location location :message message))

(defclass route-summary (proto-message)
  ((point-count
    :initarg :point-count
    :initform 0
    :accessor route-summary-point-count
    :field 1
    :proto-type :int32
    :documentation "Number of points received")
   (feature-count
    :initarg :feature-count
    :initform 0
    :accessor route-summary-feature-count
    :field 2
    :proto-type :int32
    :documentation "Number of known features passed")
   (distance
    :initarg :distance
    :initform 0
    :accessor route-summary-distance
    :field 3
    :proto-type :int32
    :documentation "Distance in meters")
   (elapsed-time
    :initarg :elapsed-time
    :initform 0
    :accessor route-summary-elapsed-time
    :field 4
    :proto-type :int32
    :documentation "Duration in seconds"))
  (:metaclass proto-metaclass)
  (:documentation "Summary of a recorded route"))

(defun make-route-summary (&key (point-count 0) (feature-count 0)
                                (distance 0) (elapsed-time 0))
  "Create a RouteSummary instance."
  (make-instance 'route-summary
                 :point-count point-count
                 :feature-count feature-count
                 :distance distance
                 :elapsed-time elapsed-time))
