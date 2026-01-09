#!/bin/bash
# run-compile-test.sh - Run compilation test with dependencies

sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(format t "~%Loading dependencies...~%")' \
  --eval '(asdf:load-system :babel :verbose nil)' \
  --eval '(asdf:load-system :bordeaux-threads :verbose nil)' \
  --eval '(asdf:load-system :usocket :verbose nil)' \
  --eval '(asdf:load-system :cl+ssl :verbose nil)' \
  --eval '(asdf:load-system :alexandria :verbose nil)' \
  --load test-compile.lisp
