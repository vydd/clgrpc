;;;; server-clos.lisp - RouteGuide gRPC server using CLOS-based API
;;;;
;;;; Demonstrates all four types of gRPC calls with CLOS services:
;;;; - GetFeature: Unary RPC
;;;; - ListFeatures: Server streaming RPC
;;;; - RecordRoute: Client streaming RPC
;;;; - RouteChat: Bidirectional streaming RPC

;; Load Quicklisp
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Install dependencies
(ql:quickload '(:cl+ssl :usocket :bordeaux-threads
                :alexandria :trivial-gray-streams :fast-io :babel :cl-json)
              :silent t)

;; Add current directory to ASDF registry
(push (make-pathname :name nil :type nil
                     :defaults (merge-pathnames "../../" *load-truename*))
      asdf:*central-registry*)

;; Load the system
(format t "~%Loading clgrpc...~%")
(asdf:load-system :clgrpc)

;; Load RouteGuide protobuf definitions
(load (merge-pathnames "routeguide-proto.lisp" *load-truename*))

(in-package #:clgrpc.grpc)

(defparameter *route-features* nil
  "List of known features loaded from database")

(defparameter *route-notes* (make-hash-table :test 'equal)
  "Map from location to list of route notes at that location")

;;; Helper functions

(defun load-features (filename)
  "Load features from JSON file."
  (with-open-file (in filename)
    (let ((json (cl-json:decode-json in)))
      (mapcar (lambda (item)
                (let ((location (cdr (assoc :location item)))
                      (name (cdr (assoc :name item))))
                  (routeguide:make-feature
                   :name (or name "")
                   :location (routeguide:make-point
                              :latitude (cdr (assoc :latitude location))
                              :longitude (cdr (assoc :longitude location))))))
              json))))

(defun point-equal (p1 p2)
  "Check if two points are equal."
  (and (= (routeguide:point-latitude p1)
          (routeguide:point-latitude p2))
       (= (routeguide:point-longitude p1)
          (routeguide:point-longitude p2))))

(defun point-hash (point)
  "Generate hash key for a point."
  (format nil "~D,~D"
          (routeguide:point-latitude point)
          (routeguide:point-longitude point)))

(defun in-range (point rect)
  "Check if point is inside rectangle."
  (let* ((lo (routeguide:rectangle-lo rect))
         (hi (routeguide:rectangle-hi rect))
         (lat (routeguide:point-latitude point))
         (lon (routeguide:point-longitude point))
         (left (min (routeguide:point-longitude lo)
                    (routeguide:point-longitude hi)))
         (right (max (routeguide:point-longitude lo)
                     (routeguide:point-longitude hi)))
         (top (max (routeguide:point-latitude lo)
                   (routeguide:point-latitude hi)))
         (bottom (min (routeguide:point-latitude lo)
                      (routeguide:point-latitude hi))))
    (and (>= lat bottom) (<= lat top)
         (>= lon left) (<= lon right))))

(defun calc-distance (p1 p2)
  "Calculate distance between two points in meters using Haversine formula."
  (let* ((coord-factor (/ 1.0 1e7))
         (lat1 (* (routeguide:point-latitude p1) coord-factor))
         (lat2 (* (routeguide:point-latitude p2) coord-factor))
         (lon1 (* (routeguide:point-longitude p1) coord-factor))
         (lon2 (* (routeguide:point-longitude p2) coord-factor))
         (lat-rad1 (* lat1 (/ pi 180)))
         (lat-rad2 (* lat2 (/ pi 180)))
         (dlat (* (- lat2 lat1) (/ pi 180)))
         (dlon (* (- lon2 lon1) (/ pi 180)))
         (a (+ (* (sin (/ dlat 2)) (sin (/ dlat 2)))
               (* (cos lat-rad1) (cos lat-rad2)
                  (sin (/ dlon 2)) (sin (/ dlon 2)))))
         (c (* 2 (atan (sqrt a) (sqrt (- 1 a)))))
         (r 6371000)) ; Earth radius in meters
    (round (* r c))))

;;; Define the RouteGuide service using CLOS

(defclass route-guide-service (grpc-service)
  ()
  (:metaclass grpc-service-metaclass)
  (:service-name "routeguide.RouteGuide")
  (:package "routeguide")
  (:documentation "RouteGuide service definition"))

;; GetFeature - Unary RPC
;; :method-name defaults to "GetFeature", :rpc-type defaults to :unary
(defgrpc-method get-feature ((service route-guide-service)
                             (request routeguide:point)
                             context)
  (:documentation "Get the feature at the given point")

  (declare (ignore service context))

  (format *error-output* "GetFeature: lat=~D lon=~D~%"
          (routeguide:point-latitude request)
          (routeguide:point-longitude request))

  ;; Find feature at this location
  (let ((feature (find-if (lambda (f)
                           (point-equal request
                                       (routeguide:feature-location f)))
                         *route-features*)))
    (if feature
        feature
        ;; No feature found - return unnamed feature
        (routeguide:make-feature :name "" :location request))))

;; ListFeatures - Server streaming RPC
(defgrpc-method list-features ((service route-guide-service)
                               (request routeguide:rectangle)
                               context)
  (:rpc-type :server-streaming)
  (:documentation "List all features in the given rectangle")

  (declare (ignore service))

  (format *error-output* "ListFeatures: rectangle~%")

  (let ((stream (clgrpc.server:get-stream context)))
    ;; Stream all features in the rectangle
    (dolist (feature *route-features*)
      (when (in-range (routeguide:feature-location feature) request)
        (format *error-output* "  Sending: ~A~%"
                (routeguide:feature-name feature))
        (clgrpc.server:server-stream-send stream
                                         (proto-serialize feature))))

    (values +grpc-status-ok+ nil nil)))

;; RecordRoute - Client streaming RPC
(defgrpc-method record-route ((service route-guide-service)
                              (request routeguide:point) ; Not used - streaming
                              context)
  (:rpc-type :client-streaming)
  (:response-type routeguide:route-summary)
  (:documentation "Record a route composed of points")

  (declare (ignore service request))

  (format *error-output* "RecordRoute: receiving points~%")

  (let ((stream (clgrpc.server:get-stream context))
        (point-count 0)
        (feature-count 0)
        (distance 0)
        (start-time (get-internal-real-time))
        (previous-point nil))

    ;; Receive all points
    (loop for msg-bytes = (clgrpc.server:server-stream-recv stream)
          while msg-bytes
          do (let ((point (proto-deserialize 'routeguide:point msg-bytes)))
               (incf point-count)
               (format *error-output* "  Point ~D: lat=~D lon=~D~%"
                       point-count
                       (routeguide:point-latitude point)
                       (routeguide:point-longitude point))

               ;; Check if there's a feature here
               (when (find-if (lambda (f)
                               (point-equal point
                                           (routeguide:feature-location f)))
                             *route-features*)
                 (incf feature-count))

               ;; Calculate distance from previous point
               (when previous-point
                 (incf distance (calc-distance previous-point point)))

               (setf previous-point point)))

    (let* ((end-time (get-internal-real-time))
           (elapsed-seconds (round (/ (- end-time start-time)
                                    internal-time-units-per-second))))
      (format *error-output* "  Summary: ~D points, ~D features, ~D meters, ~D seconds~%"
              point-count feature-count distance elapsed-seconds)

      (values (proto-serialize
               (routeguide:make-route-summary
                :point-count point-count
                :feature-count feature-count
                :distance distance
                :elapsed-time elapsed-seconds))
              +grpc-status-ok+ nil nil))))

;; RouteChat - Bidirectional streaming RPC
(defgrpc-method route-chat ((service route-guide-service)
                            (request routeguide:route-note) ; Not used - streaming
                            context)
  (:rpc-type :bidirectional)
  (:documentation "Chat about a route")

  (declare (ignore service request))

  (format *error-output* "RouteChat: starting chat~%")

  (let ((stream (clgrpc.server:get-stream context)))
    ;; Receive notes and send back previous notes at same location
    (loop for msg-bytes = (clgrpc.server:server-stream-recv stream)
          while msg-bytes
          do (let ((note (proto-deserialize 'routeguide:route-note msg-bytes)))
               (format *error-output* "  Received note at lat=~D lon=~D: ~A~%"
                       (routeguide:point-latitude
                        (routeguide:route-note-location note))
                       (routeguide:point-longitude
                        (routeguide:route-note-location note))
                       (routeguide:route-note-message note))

               ;; Get key for this location
               (let ((key (point-hash (routeguide:route-note-location note))))

                 ;; Send all previous notes at this location
                 (dolist (prev-note (gethash key *route-notes*))
                   (format *error-output* "    Sending previous note: ~A~%"
                           (routeguide:route-note-message prev-note))
                   (clgrpc.server:server-stream-send stream
                                                    (proto-serialize prev-note)))

                 ;; Store this note for future queries
                 (push note (gethash key *route-notes*)))))

    (values +grpc-status-ok+ nil nil)))

;;; Main

(defun main ()
  "Start RouteGuide server."
  (format t "~%RouteGuide gRPC Server (CLOS-based API)~%")
  (format t "==========================================~%~%")

  ;; Load feature database
  (format t "Loading features...~%")
  (setf *route-features* (load-features
                   (merge-pathnames "route_guide_db.json" *load-truename*)))
  (format t "Loaded ~D features~%~%" (length *route-features*))

  ;; Create server
  (let ((server (clgrpc.server:make-server :port 50051)))

    ;; Create service instance and register it
    ;; This automatically registers all 4 methods!
    (let ((route-guide (make-instance 'route-guide-service)))
      (clgrpc.server:register-service (clgrpc.server:grpc-server-router server)
                                      route-guide))

    (format t "~%")

    ;; Start server
    (format t "Starting server on port 50051...~%")
    (clgrpc.server:start-server server)
    (format t "~%Server listening on port 50051~%")
    (format t "Press Ctrl+C to stop~%~%")

    ;; Keep running
    (handler-case
        (loop (sleep 1))
      (sb-sys:interactive-interrupt ()
        (format t "~%Stopping server...~%")
        (clgrpc.server:stop-server server)
        (format t "Server stopped~%")))))

;; Run if called with --run
(when (member "--run" sb-ext:*posix-argv* :test #'string=)
  (main))
