#!/bin/sh

HOSTNAME=`hostname --short`

if [ "$HOSTNAME" = 'simba' ]; then
  # launch compton
    compton -b --backend glx --vsync opengl-swc --glx-no-stencil --glx-no-rebind-pixmap --glx-swap-method 0 --fading --no-fading-openclose --dbus
    ~/.screenlayout/desktop.sh
fi

