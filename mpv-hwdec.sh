#!/bin/sh

HOSTNAME=`hostname --short`

# Use vaapi-copy for Wayland
sh -c 'if [ "$HOSTNAME" = ''nala'' ]; then echo ''hwdec=vaapi-copy''; else echo ''hwdec=auto''; fi > ~/.config/mpv/hwdec.conf'

