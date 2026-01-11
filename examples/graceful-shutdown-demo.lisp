;;;; graceful-shutdown-demo.lisp - Demonstration of graceful server shutdown
;;;;
;;;; This example shows how the server gracefully shuts down:
;;;; 1. Stops accepting new connections
;;;; 2. Sends GOAWAY to existing clients
;;;; 3. Waits for active requests to complete
;;;; 4. Force-closes connections after timeout

(ql:quickload '(:clgrpc :clgrpc-examples) :silent t)

(defun demo-graceful-shutdown ()
  "Demonstrate graceful shutdown with the RouteGuide service"

  (format t "~%╔════════════════════════════════════════════════════════╗~%")
  (format t "║  Graceful Shutdown Demo                                ║~%")
  (format t "╚════════════════════════════════════════════════════════╝~%~%")

  ;; Create server
  (let ((server (clgrpc:make-server :port 50051)))

    ;; Load RouteGuide data
    (let ((features (clgrpc-examples::routeguide-clos-load-features
                     #P"/home/vydd/Code/clgrpc/examples/routeguide/route_guide_db.json")))

      (format t "1. Starting server with ~D features...~%" (length features))

      ;; Register RouteGuide service
      (let ((route-guide (make-instance 'clgrpc-examples::route-guide-service-clos)))
        (clgrpc:register-service (clgrpc.server:grpc-server-router server) route-guide))

      (clgrpc:start-server server)
      (format t "   ✓ Server ready on port 50051~%~%")

      ;; Simulate some activity
      (format t "2. Server is now handling requests...~%")
      (format t "   (Press Ctrl+C to test graceful shutdown)~%~%")

      ;; Setup signal handler for Ctrl+C
      (handler-case
          (loop
            (sleep 1))
        (sb-sys:interactive-interrupt ()
          (format t "~%~%3. Received shutdown signal!~%~%")

          ;; Graceful shutdown with 30 second timeout
          (clgrpc:stop-server server :timeout 30)

          (format t "~%✓ Server shutdown complete!~%~%"))))))

;; Usage Instructions
(defun print-usage ()
  (format t "~%═══════════════════════════════════════════════════════════~%")
  (format t "  Graceful Shutdown Usage~%")
  (format t "═══════════════════════════════════════════════════════════~%~%")

  (format t "Basic usage:~%")
  (format t "  (stop-server server)                  ; Default 30s timeout~%")
  (format t "  (stop-server server :timeout 60)      ; Custom timeout~%~%")

  (format t "What happens during graceful shutdown:~%")
  (format t "  1. Stop accepting new connections~%")
  (format t "  2. Send GOAWAY to all connected clients~%")
  (format t "  3. Wait for active requests to complete (up to timeout)~%")
  (format t "  4. Force-close remaining connections after timeout~%~%")

  (format t "Client behavior:~%")
  (format t "  - Active requests complete normally~%")
  (format t "  - New requests get a fresh connection~%")
  (format t "  - No dropped requests during deployment~%~%")

  (format t "Production deployment:~%")
  (format t "  1. Send SIGTERM to server process~%")
  (format t "  2. Server initiates graceful shutdown~%")
  (format t "  3. Load balancer removes server from rotation~%")
  (format t "  4. Active requests complete~%")
  (format t "  5. Server exits cleanly~%~%")

  (format t "═══════════════════════════════════════════════════════════~%~%"))

(print-usage)

;; Run demo
(demo-graceful-shutdown)
