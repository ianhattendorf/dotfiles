#!/bin/sh

HOSTNAME=`hostname --short`

# Use vaapi-copy for Wayland
# Also set gpu-context
if [ "$HOSTNAME" = 'nala' ]; then
  echo 'hwdec=vaapi-copy
gpu-context=wayland'
elif [ "$HOSTNAME" = 'simba' ]; then
  echo 'hwdec=auto
gpu-context=wayland
dither-depth=10'
else
  echo 'hwdec=auto'
fi > ~/.config/mpv/hwdec.conf
