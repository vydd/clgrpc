;;;; run-tests.lisp - Script to load dependencies and run tests

;; Load Quicklisp (check both .quicklisp and quicklisp directories)
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Install dependencies if needed
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
(fiveam:run! 'clgrpc-tests:clgrpc-all)
