#!/bin/sh
set -e

if [ -z "$key" ]; then
    echo "Error! Pass your VimGolf key into the container as an environment variable:\n  -e \"key=caa2d86f5...\""
    exit 1
fi

echo $key | vimgolf setup
vimgolf put "$@"

