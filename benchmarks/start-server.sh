#!/bin/bash
cd /home/vydd/Code/clgrpc/benchmarks
sbcl --noinform --load benchmark-cl.lisp > /tmp/server.log 2>&1 &
