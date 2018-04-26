#!/bin/sh

HOSTNAME=`hostname`

if [ "$HOSTNAME" = 'work-vm' ]; then
  # launch compton
  compton -b
fi

