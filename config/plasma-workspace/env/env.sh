#!/bin/sh

export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:=$HOME/.config}"
export GPG_TTY=$(tty)
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh"
export HOSTNAME_SHORT=$(hostname --short)
export WEECHAT_HOME="$XDG_CONFIG_HOME/weechat"
gpg-connect-agent updatestartuptty /bye

HOSTNAME=`hostname --short`

if [ "$HOSTNAME" = 'nala' ]; then
  export GDK_DPI_SCALE=1.25
  export GDK_SCALE=1.25
fi
