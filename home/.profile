# .profile

export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:=$HOME/.config}"
export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket
export HOSTNAME_SHORT=$(hostname --short)
export WEECHAT_HOME="$XDG_CONFIG_HOME/weechat"

# User specific environment and startup programs

PATH=$PATH:$HOME/.local/bin:$HOME/go/bin:$HOME/bin

if [ -f ~/.cargo/env ]; then
  . ~/.cargo/env
fi

export NVM_DIR="$HOME/.config/nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"

if [ -f /usr/local/opt/nvm/nvm.sh ]; then
  . /usr/local/opt/nvm/nvm.sh
fi

export EDITOR=vim
export PATH
. "$HOME/.cargo/env"

if [ -z "$DISPLAY" ] && [ -z "$WAYLAND_DISPLAY" ]; then
  case "${XDG_VTNR:-0}" in
    1|4)
      exec start-sway
      ;;
    5)
      exec startx
      ;;
  esac
fi
