;;;; run-tests.lisp - Script to run the clgrpc test suite
;;;;
;;;; Prerequisites:
;;;;   The clgrpc system must be available to ASDF. Options:
;;;;   1. Symlink: ln -s /path/to/clgrpc ~/quicklisp/local-projects/clgrpc
;;;;   2. ASDF config: Add to ~/.config/common-lisp/source-registry.conf.d/
;;;;
;;;; Usage:
;;;;   sbcl --load run-tests.lisp
;;;;   # Or directly:
;;;;   sbcl --eval '(ql:quickload :clgrpc-tests)' --eval '(asdf:test-system :clgrpc)'

;; Load and run tests
(format t "~%Loading clgrpc and clgrpc-tests...~%")
(ql:quickload :clgrpc-tests :silent t)

(format t "~%Running tests...~%~%")
(asdf:test-system :clgrpc)
