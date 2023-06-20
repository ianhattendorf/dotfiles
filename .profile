# .profile

export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:=$HOME/.config}"
# export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh
export HOSTNAME_SHORT=$(hostname --short)
export WEECHAT_HOME="$XDG_CONFIG_HOME/weechat"

# User specific environment and startup programs

PATH=$PATH:$HOME/.local/bin:$HOME/bin

if [ -f ~/.cargo/env ]; then
  . ~/.cargo/env
fi

if [ -d $HOME/.nvm ]; then
  export NVM_DIR="$HOME/.nvm"
fi

if [ -f /usr/local/opt/nvm/nvm.sh ]; then
  . /usr/local/opt/nvm/nvm.sh
fi

export EDITOR=vim
export PATH
. "$HOME/.cargo/env"
