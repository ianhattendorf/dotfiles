#!/bin/sh

if [ "$#" -ne 1 ]
then
  echo "Usage: $0 <version>"
  exit 1
fi

antigen_version="$1"
antigen_dir="$HOME/bin"

antigen_url="https://github.com/zsh-users/antigen/releases/download/v$antigen_version/antigen.zsh"
antigen_path="$antigen_dir/antigen-$antigen_version.zsh"

if [ ! -f "$antigen_path" ]; then
  mkdir -p "$antigen_dir"
  curl -L "$antigen_url" > "$antigen_path"
fi

