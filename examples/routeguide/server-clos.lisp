;;;; server-clos.lisp - RouteGuide gRPC server using CLOS-based API
;;;;
;;;; Demonstrates all four types of gRPC calls with CLOS services:
;;;; - GetFeature: Unary RPC
;;;; - ListFeatures: Server streaming RPC
;;;; - RecordRoute: Client streaming RPC
;;;; - RouteChat: Bidirectional streaming RPC
;;;;
;;;; Usage:
;;;;   (ql:quickload :clgrpc-examples)
;;;;   (clgrpc-examples:routeguide-server-clos-main)

(in-package #:clgrpc-examples)

;;; Note: This example uses internal clgrpc packages for advanced features
;;; like streaming context access.

(defparameter *route-features-clos* nil
  "List of known features loaded from database")

(defparameter *route-notes-clos* (make-hash-table :test 'equal)
  "Map from location to list of route notes at that location")

;;; Helper functions (reused from server.lisp)

(defun routeguide-clos-load-features (filename)
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

(defun routeguide-clos-point-equal (p1 p2)
  "Check if two points are equal."
  (and (= (routeguide:point-latitude p1)
          (routeguide:point-latitude p2))
       (= (routeguide:point-longitude p1)
          (routeguide:point-longitude p2))))

(defun routeguide-clos-point-hash (point)
  "Generate hash key for a point."
  (format nil "~D,~D"
          (routeguide:point-latitude point)
          (routeguide:point-longitude point)))

(defun routeguide-clos-in-range (point rect)
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

(defun routeguide-clos-calc-distance (p1 p2)
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

(defclass route-guide-service-clos (grpc-service)
  ()
  (:metaclass grpc-service-metaclass)
  (:service-name "routeguide.RouteGuide")
  (:package "routeguide")
  (:documentation "RouteGuide service definition"))

;; GetFeature - Unary RPC
(defgrpc-method get-feature ((service route-guide-service-clos)
                             (request routeguide:point)
                             context)
  (:documentation "Get the feature at the given point")
  (declare (ignore service context))
  (clgrpc.http2:debug-log "GetFeature: lat=~D lon=~D~%"
                          (routeguide:point-latitude request)
                          (routeguide:point-longitude request))
  (let ((feature (find-if (lambda (f)
                            (routeguide-clos-point-equal request
                                                         (routeguide:feature-location f)))
                          *route-features-clos*)))
    (if feature
        feature
        (routeguide:make-feature :name "" :location request))))

;; ListFeatures - Server streaming RPC
(defgrpc-method list-features ((service route-guide-service-clos)
                               (request routeguide:rectangle)
                               context)
  (:rpc-type :server-streaming)
  (:documentation "List all features in the given rectangle")
  (declare (ignore service))
  (clgrpc.http2:debug-log "ListFeatures: rectangle~%")
  (let ((stream (clgrpc.server:get-stream context)))
    (dolist (feature *route-features-clos*)
      (when (routeguide-clos-in-range (routeguide:feature-location feature) request)
        (clgrpc.http2:debug-log "  Sending: ~A~%"
                                (routeguide:feature-name feature))
        (server-stream-send stream (proto-serialize feature))))
    (values +grpc-status-ok+ nil nil)))

;; RecordRoute - Client streaming RPC
(defgrpc-method record-route ((service route-guide-service-clos)
                              (request routeguide:point)
                              context)
  (:rpc-type :client-streaming)
  (:response-type routeguide:route-summary)
  (:documentation "Record a route composed of points")
  (declare (ignore service request))
  (clgrpc.http2:debug-log "RecordRoute: receiving points~%")
  (let ((stream (clgrpc.server:get-stream context))
        (point-count 0)
        (feature-count 0)
        (distance 0)
        (start-time (get-internal-real-time))
        (previous-point nil))
    (loop for msg-bytes = (server-stream-recv stream)
          while msg-bytes
          do (let ((point (proto-deserialize 'routeguide:point msg-bytes)))
               (incf point-count)
               (clgrpc.http2:debug-log "  Point ~D: lat=~D lon=~D~%"
                                       point-count
                                       (routeguide:point-latitude point)
                                       (routeguide:point-longitude point))
               (when (find-if (lambda (f)
                                (routeguide-clos-point-equal point
                                                             (routeguide:feature-location f)))
                              *route-features-clos*)
                 (incf feature-count))
               (when previous-point
                 (incf distance (routeguide-clos-calc-distance previous-point point)))
               (setf previous-point point)))
    (let* ((end-time (get-internal-real-time))
           (elapsed-seconds (round (/ (- end-time start-time)
                                      internal-time-units-per-second))))
      (clgrpc.http2:debug-log "  Summary: ~D points, ~D features, ~D meters, ~D seconds~%"
                              point-count feature-count distance elapsed-seconds)
      (values (proto-serialize
               (routeguide:make-route-summary
                :point-count point-count
                :feature-count feature-count
                :distance distance
                :elapsed-time elapsed-seconds))
              +grpc-status-ok+ nil nil))))

;; RouteChat - Bidirectional streaming RPC (using with-bidirectional-stream)
(defgrpc-method route-chat ((service route-guide-service-clos)
                            (request routeguide:route-note)
                            context)
  (:rpc-type :bidirectional)
  (:documentation "Chat about a route using true concurrent bidirectional streaming")
  (declare (ignore service request))
  (clgrpc.http2:debug-log "RouteChat: starting chat (with concurrent send/recv)~%")

  ;; Use the bidirectional streaming macro for true concurrent send/receive
  (with-bidirectional-stream (send-note recv-note) context
    ;; Loop receiving messages until client closes stream (recv-note returns nil)
    ;; Don't use timeout - wait indefinitely for next message
    (loop for msg-bytes = (recv-note)  ; Block until next message or stream closed
          while msg-bytes
          do (let ((note (proto-deserialize 'routeguide:route-note msg-bytes)))
               (clgrpc.http2:debug-log "  Received note at lat=~D lon=~D: ~A~%"
                                       (routeguide:point-latitude
                                        (routeguide:route-note-location note))
                                       (routeguide:point-longitude
                                        (routeguide:route-note-location note))
                                       (routeguide:route-note-message note))
               ;; Send all previous notes at this location
               (let ((key (routeguide-clos-point-hash (routeguide:route-note-location note))))
                 (dolist (prev-note (gethash key *route-notes-clos*))
                   (clgrpc.http2:debug-log "    Sending previous note: ~A~%"
                                           (routeguide:route-note-message prev-note))
                   (send-note (proto-serialize prev-note)))
                 ;; Store this note for future chats
                 (push note (gethash key *route-notes-clos*))))))

  (values +grpc-status-ok+ nil nil))

;;; Main

(defun routeguide-server-clos-main (&optional (db-path nil))
  "Start RouteGuide server using CLOS-based service.
DB-PATH is the path to route_guide_db.json. If not provided, looks in
the examples/routeguide directory."
  (format t "~%RouteGuide gRPC Server (CLOS-based API)~%")
  (format t "==========================================~%~%")

  ;; Load feature database
  (format t "Loading features...~%")
  (let ((db-file (or db-path
                     (asdf:system-relative-pathname :clgrpc-examples
                                                    "routeguide/route_guide_db.json"))))
    (setf *route-features-clos* (routeguide-clos-load-features db-file)))
  (format t "Loaded ~D features~%~%" (length *route-features-clos*))

  ;; Reset route notes
  (clrhash *route-notes-clos*)

  ;; Create server
  (let ((server (make-server :port 50051)))
    ;; Create service instance and register it
    ;; This automatically registers all 4 methods!
    (let ((route-guide (make-instance 'route-guide-service-clos)))
      (register-service (grpc-server-router server) route-guide))
    (format t "~%")
    (format t "Starting server on port 50051...~%")
    (start-server server)
    (format t "~%Server listening on port 50051~%")
    (format t "Press Ctrl+C to stop~%~%")
    (handler-case
        (loop (sleep 1))
      (#+sbcl sb-sys:interactive-interrupt
       #+ccl ccl:interrupt-signal-condition
       #-(or sbcl ccl) error ()
        (format t "~%Stopping server...~%")
        (stop-server server)
        (format t "Server stopped~%")))))
