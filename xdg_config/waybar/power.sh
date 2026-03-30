#!/bin/sh

sudo turbostat --quiet --Summary -s PkgWatt | awk '{printf("%.1f", $1)}'
