#!/bin/sh

pkill -SIGUSR1 foot
pkill -WINCH -u "$USER" -x tmux 2>/dev/null || true
