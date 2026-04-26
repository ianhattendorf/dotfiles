#!/bin/sh

pkill -SIGUSR2 foot
pkill -WINCH -u "$USER" -x tmux 2>/dev/null || true
