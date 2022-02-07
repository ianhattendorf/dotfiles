#!/bin/sh

set -eu

if [ $(uname) == 'Darwin' ]; then
  alias readlink=greadlink
fi

basedir=$(dirname "$( readlink -f -- "$0")")
[ $(hostname) == 'ianh-dell.axonet.local' ] && config_type='work' || config_type='home'

ln -fs "$basedir/.gitconfig-local-$config_type" ~/.gitconfig-local
