#!/bin/sh

# Need to redirect output here, because capture_stdout() in Ruby
# doesn't cover calls using Kernel.system().

diff -u "$@" >"$HOME/diff-output.txt"
