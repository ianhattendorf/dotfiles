#!/bin/sh

set -eu

if ! command -v gem >/dev/null 2>&1; then
  >&2 echo "RubyGems is required to install tmuxinator"
  exit 1
fi

if gem list -i tmuxinator >/dev/null 2>&1; then
  exit 0
fi

gem install --user-install tmuxinator
