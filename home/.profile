# .profile

export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:=$HOME/.config}"
if [ -r "$XDG_CONFIG_HOME/dotfiles/session-env.sh" ]; then
  . "$XDG_CONFIG_HOME/dotfiles/session-env.sh"
fi
if [ -z "${SSH_AUTH_SOCK:-}" ] && [ -n "${XDG_RUNTIME_DIR:-}" ]; then
  export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
fi
export HOSTNAME_SHORT=$(hostname -s)
export WEECHAT_HOME="$XDG_CONFIG_HOME/weechat"

# User specific environment and startup programs

PATH=$PATH:$HOME/.local/bin:$HOME/go/bin:$HOME/bin

if [ -f ~/.cargo/env ]; then
  . ~/.cargo/env
fi

export NVM_DIR="$HOME/.config/nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"

export EDITOR=vim
export PATH
if [ -f "$HOME/.cargo/env" ]; then
  . "$HOME/.cargo/env"
fi

if [ -x /opt/homebrew/bin/brew ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

if [ -z "$DISPLAY" ] && [ -z "$WAYLAND_DISPLAY" ] && [ -z "${TMUX:-}" ] && [ -z "${SSH_CONNECTION:-}" ]; then
  case "${XDG_VTNR:-0}" in
    1|4)
      exec start-sway
      ;;
    5)
      exec startx
      ;;
  esac
fi
