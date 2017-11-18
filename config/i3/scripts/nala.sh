#!/bin/sh

HOSTNAME=`hostname`

if [ "$HOSTNAME" = 'nala' ]; then
  # get touchpad xinput id
  TOUCHPAD_ID=`xinput list | grep 'Elantech Touchpad' | awk -F'=' '{print $2}' | grep -Po '^\d+'`
  # set touchpad Tapping Enabled
  xinput set-prop $TOUCHPAD_ID 'libinput Tapping Enabled' 1
fi

