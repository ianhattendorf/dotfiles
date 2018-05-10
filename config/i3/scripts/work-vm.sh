#!/bin/sh

HOSTNAME=`hostname --short`

if [ "$HOSTNAME" = 'work-vm' ]; then
  # launch compton
  compton -b --fading --no-fading-openclose --dbus
fi

