#!/bin/sh

HOSTNAME=`hostname --short`

if [ "$HOSTNAME" = 'nala' ]; then
  # get touchpad xinput id
  TOUCHPAD_ID=`xinput list | grep 'Elantech Touchpad' | awk -F'=' '{print $2}' | grep -Po '^\d+'`
  # set touchpad Tapping Enabled
  xinput set-prop $TOUCHPAD_ID 'libinput Tapping Enabled' 1
  # launch compton
  compton -b --backend glx --vsync opengl-swc --glx-no-stencil --glx-no-rebind-pixmap --glx-swap-method 0
fi

