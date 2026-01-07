;;;; run-tests.lisp - Script to load dependencies and run tests

;; Load Quicklisp
(load "~/quicklisp/setup.lisp" :if-does-not-exist nil)

;; Install dependencies if needed (Phase 1 only needs these)
;; Note: cl-protobufs will be added when available - not in Quicklisp
(ql:quickload '(:cl+ssl :usocket :bordeaux-threads
                :alexandria :trivial-gray-streams :fast-io :babel :fiveam)
              :silent t)

;; Add current directory to ASDF registry
(push (truename ".") asdf:*central-registry*)

;; Load the system
(format t "~%Loading clgrpc...~%")
(asdf:load-system :clgrpc)

;; Load tests
(format t "~%Loading clgrpc-tests...~%")
(asdf:load-system :clgrpc-tests)

;; Run tests
(format t "~%Running tests...~%~%")
(fiveam:run! 'clgrpc-tests:frame-tests)
