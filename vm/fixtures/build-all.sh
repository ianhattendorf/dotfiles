#!/bin/sh
# Rebuilds every scenario fixture from scratch. Generation is cheap (tiny repos), so
# "reset" and "build" are the same operation — there's no in-place fast path to maintain.
set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
root=${1:-$HOME/dev/test}

mkdir -p "$root"
sh "$here/stash-conflict.sh" "$root/stash-conflict-repro"
sh "$here/rebase-multi-conflict.sh" "$root/rebase-multi-conflict"
