function initAntigen {
  if [ ! -f "$1" ]; then
    >&2 echo "Failed to init antigen, not found at: $1"
    return 1
  fi

  source "$1"

  antigen use oh-my-zsh

  antigen bundles <<EOBUNDLES
  command-not-found
  nvm
  rupa/z
  zsh-users/zsh-autosuggestions
  zsh-users/zsh-syntax-highlighting
EOBUNDLES

  antigen theme kphoen

  antigen apply
}

function initAutocomplete {
  fpath=(~/.local/share/zsh/site-functions $fpath)
  autoload -U +X compinit && compinit
  if [ -f ~/.local/share/zsh/site-functions/_tmuxinator ]; then
    autoload -U +X _tmuxinator
    compdef _tmuxinator tmuxinator
  fi
  autoload -U +X bashcompinit && bashcompinit

  # terraform
  if [ -f /usr/bin/terraform ]; then
    complete -o nospace -C /usr/bin/terraform terraform
  fi

  # bun completions
  [ -s "/home/ian/.bun/_bun" ] && source "/home/ian/.bun/_bun"

  eval "$(git wt --init zsh)"
}

function initTheme {
  if hash darkman 2>/dev/null; then
    theme="$(darkman get 2>/dev/null || true)"
    case "$theme" in
      dark)
        if [ -f ~/.local/share/dark-mode.d/dark.sh ]; then
          ~/.local/share/dark-mode.d/dark.sh
        fi
        ;;
      light)
        if [ -f ~/.local/share/light-mode.d/light.sh ]; then
          ~/.local/share/light-mode.d/light.sh
        fi
        ;;
    esac
  fi
}

function initAliases {
  alias mux='tmuxinator'
}

function initOs {
  case `uname` in
    Linux)
      initAntigen ~/bin/antigen-2.2.3.zsh
      initAutocomplete
      initTheme
      ;;
  esac
}

function initTmux {
  function syncFromTmuxEnvironment {
    local name="$1"
    local tmux_env current_value

    tmux_env="$(tmux show-environment "$name" 2>/dev/null)" || return 0

    case "$tmux_env" in
      -"$name")
        unset "$name"
        ;;
      "$name="*)
        current_value="${(P)name-}"
        if [ "$current_value" != "${tmux_env#*=}" ]; then
          typeset -gx "$tmux_env"
        fi
        ;;
    esac
  }

  if [ -n "$TMUX" ]; then
    tmux set-environment -g PATH "$PATH"
    tmux set-environment -g SSH_AUTH_SOCK "${SSH_AUTH_SOCK:-}"
    syncFromTmuxEnvironment SSH_AUTH_SOCK
    syncFromTmuxEnvironment SWAYSOCK
    syncFromTmuxEnvironment WAYLAND_DISPLAY
    syncFromTmuxEnvironment DISPLAY
  fi
}

function init {
  initOs
  initAliases
  initTmux
}

init
