#!/bin/sh

HOSTNAME=`hostname --short`

# Currently same, maybe change nala to vdpau later
sh -c 'if [ "$HOSTNAME" = ''nala'' ]; then echo ''hwdec=auto''; else echo ''hwdec=auto''; fi > ~/.config/mpv/hwdec.conf'

