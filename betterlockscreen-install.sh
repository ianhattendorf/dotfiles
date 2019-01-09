#!/bin/sh

set -eu

if [ "$#" -ne 1 ]
then
  >&2 echo "Usage: $0 <version>"
  exit 1
fi

if [ `uname` == 'Darwin' ]; then
  >&2 echo "Not available on macOS"
  exit 0
fi

version="$1"
dir="$HOME/bin"

#url="https://github.com/pavanjadhaw/betterlockscreen/archive/$version.tar.gz"
# Temporarily use fork until PR merged
url="https://github.com/ianhattendorf/betterlockscreen/archive/$version.tar.gz"
path="$dir/betterlockscreen"
path_version="$path-$version"

if [ ! -f "$path_version" ]; then
  mkdir -p "$dir"
  curl -L "$url" | tar xzv --strip-components=1 -C "$dir" "betterlockscreen-$version/betterlockscreen" && mv "$dir/betterlockscreen" "$path_version"
  ln -s "$path_version" "$path" 
fi

