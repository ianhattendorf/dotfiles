#!/bin/sh

HOSTNAME=`hostname`

if [ "$HOSTNAME" = nala ]; then
  # set touchpad Tapping Enabled
  xinput set-prop 13 280 1
fi

