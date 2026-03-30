#!/bin/sh

if [ -z ${WEECHAT_HOME+x}]; then
  WEECHAT_HOME=$HOME/.config/weechat
fi

env WEECHAT_PASSPHRASE=$(gpg -d "$WEECHAT_HOME/.weechat-pass.gpg") weechat
