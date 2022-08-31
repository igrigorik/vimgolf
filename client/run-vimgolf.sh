#!/bin/sh

set -e

if [ -z "$key" ]; then
  cat <<EDQ >&2
error: Missing VimGolf user key.

Pass your VimGolf key into the container as an environment variable:
  -e "key=..."

Example:
  % docker run -it --rm -e "key=<vimgolf_personal_key>" ghcr.io/filbranden/vimgolf <vimgolf_challenge_ID>
EDQ
  exit 1
fi

echo "$key" | vimgolf setup >/dev/null
exec vimgolf put "$@"
