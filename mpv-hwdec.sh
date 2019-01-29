#!/bin/sh

HOSTNAME=`hostname --short`

# Use vaapi-copy for Wayland
# Also set gpu-context
if [ "$HOSTNAME" = 'nala' ]; then
  echo 'hwdec=vaapi-copy
gpu-context=wayland
'
else
  echo 'hwdec=auto'
fi > ~/.config/mpv/hwdec.conf
