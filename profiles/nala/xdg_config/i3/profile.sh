#!/bin/sh

TOUCHPAD_ID=$(xinput list | grep 'Elantech Touchpad' | awk -F'=' '{print $2}' | grep -Po '^\d+')
xinput set-prop "$TOUCHPAD_ID" 'libinput Tapping Enabled' 1
compton -b --backend glx --vsync opengl-swc --glx-no-stencil --glx-no-rebind-pixmap --glx-swap-method 0 --fading --no-fading-openclose --dbus
