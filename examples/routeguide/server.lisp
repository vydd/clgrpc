;;;; server.lisp - RouteGuide gRPC server
;;;;
;;;; Demonstrates all four types of gRPC calls:
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

(in-package #:clgrpc.server)

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

;;; RouteGuide service handler

(defclass route-guide-handler ()
  ()
  (:documentation "Handler for RouteGuide service"))

;; GetFeature - Unary RPC
(defmethod handle-unary ((handler route-guide-handler)
                         service-name method-name
                         request-bytes context)
  (declare (ignore service-name context))
  (cond
    ((string= method-name "GetFeature")
     (let ((point (routeguide:decode-point request-bytes)))
       (format *error-output* "GetFeature: lat=~D lon=~D~%"
               (routeguide:point-latitude point)
               (routeguide:point-longitude point))

       ;; Find feature at this location
       (let ((feature (find-if (lambda (f)
                                (point-equal point
                                            (routeguide:feature-location f)))
                              *route-features*)))
         (if feature
             (values (routeguide:encode-feature feature)
                     +grpc-status-ok+ nil nil)
             ;; No feature found - return unnamed feature
             (values (routeguide:encode-feature
                      (routeguide:make-feature :name "" :location point))
                     +grpc-status-ok+ nil nil)))))

    (t
     (values nil +grpc-status-unimplemented+
             (format nil "Unknown method: ~A" method-name)
             nil))))

;; ListFeatures - Server streaming RPC
(defmethod handle-server-streaming ((handler route-guide-handler)
                                     service-name method-name
                                     request-bytes stream context)
  (declare (ignore service-name context))
  (cond
    ((string= method-name "ListFeatures")
     (let ((rect (routeguide:decode-rectangle request-bytes)))
       (format *error-output* "ListFeatures: rectangle~%")

       ;; Stream all features in the rectangle
       (dolist (feature *route-features*)
         (when (in-range (routeguide:feature-location feature) rect)
           (format *error-output* "  Sending: ~A~%"
                   (routeguide:feature-name feature))
           (server-stream-send stream
                              (routeguide:encode-feature feature))))

       (values +grpc-status-ok+ nil nil)))

    (t
     (values +grpc-status-unimplemented+
             (format nil "Unknown method: ~A" method-name)
             nil))))

;; RecordRoute - Client streaming RPC
(defmethod handle-client-streaming ((handler route-guide-handler)
                                     service-name method-name
                                     stream context)
  (declare (ignore service-name context))
  (cond
    ((string= method-name "RecordRoute")
     (format *error-output* "RecordRoute: receiving points~%")

     (let ((point-count 0)
           (feature-count 0)
           (distance 0)
           (start-time (get-internal-real-time))
           (previous-point nil))

       ;; Receive all points
       (loop for msg-bytes = (server-stream-recv stream)
             while msg-bytes
             do (let ((point (routeguide:decode-point msg-bytes)))
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

         (let ((summary (routeguide:make-route-summary
                         :point-count point-count
                         :feature-count feature-count
                         :distance distance
                         :elapsed-time elapsed-seconds)))
           (values (routeguide:encode-route-summary summary)
                   +grpc-status-ok+ nil nil)))))

    (t
     (values nil +grpc-status-unimplemented+
             (format nil "Unknown method: ~A" method-name)
             nil))))

;; RouteChat - Bidirectional streaming RPC
(defmethod handle-bidirectional-streaming ((handler route-guide-handler)
                                            service-name method-name
                                            stream context)
  (declare (ignore service-name context))
  (cond
    ((string= method-name "RouteChat")
     (format *error-output* "RouteChat: starting chat~%")

     ;; Receive notes and send back previous notes at same location
     (loop for msg-bytes = (server-stream-recv stream)
           while msg-bytes
           do (let ((note (routeguide:decode-route-note msg-bytes)))
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
                    (server-stream-send stream
                                       (routeguide:encode-route-note prev-note)))

                  ;; Store this note for future queries
                  (push note (gethash key *route-notes*)))))

     (values +grpc-status-ok+ nil nil))

    (t
     (values +grpc-status-unimplemented+
             (format nil "Unknown method: ~A" method-name)
             nil))))

;;; Main

(defun main ()
  "Start RouteGuide server."
  (format t "~%RouteGuide gRPC Server~%")
  (format t "======================~%~%")

  ;; Load feature database
  (format t "Loading features...~%")
  (setf *route-features* (load-features
                    (merge-pathnames "route_guide_db.json" *load-truename*)))
  (format t "Loaded ~D features~%~%" (length *route-features*))

  ;; Create server
  (let ((server (make-server :port 50051)))

    ;; Register RouteGuide service
    (let ((handler (make-instance 'route-guide-handler)))
      (register-handler (grpc-server-router server)
                        "routeguide.RouteGuide" "GetFeature"
                        handler :rpc-type :unary)
      (register-handler (grpc-server-router server)
                        "routeguide.RouteGuide" "ListFeatures"
                        handler :rpc-type :server-streaming)
      (register-handler (grpc-server-router server)
                        "routeguide.RouteGuide" "RecordRoute"
                        handler :rpc-type :client-streaming)
      (register-handler (grpc-server-router server)
                        "routeguide.RouteGuide" "RouteChat"
                        handler :rpc-type :bidirectional))

    (format t "Registered RouteGuide service~%~%")

    ;; Start server
    (format t "Starting server on port 50051...~%")
    (start-server server)
    (format t "~%Server listening on port 50051~%")
    (format t "Press Ctrl+C to stop~%~%")

    ;; Keep running
    (handler-case
        (loop (sleep 1))
      (sb-sys:interactive-interrupt ()
        (format t "~%Stopping server...~%")
        (stop-server server)
        (format t "Server stopped~%")))))

;; Run if called with --run
(when (member "--run" sb-ext:*posix-argv* :test #'string=)
  (main))
