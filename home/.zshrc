function fallbackPrompt {
  autoload -U colors && colors
  setopt prompt_subst
  PROMPT='[%{$fg[red]%}%n%{$reset_color%}@%{$fg[magenta]%}%m%{$reset_color%}:%{$fg[blue]%}%~%{$reset_color%}]
%# '
  RPROMPT=''
}

function clearAntigenGeneratedState {
  local adotdir="${ADOTDIR:-$HOME/.antigen}"

  rm -f -- \
    "$adotdir/.lock" \
    "$adotdir/init.zsh" \
    "$adotdir/init.zsh.zwc" \
    "$adotdir/.zcompdump" \
    "$adotdir/.zcompdump.zwc" \
    "${ZDOTDIR:-$HOME}/.zshrc.zwc"
}

function antigenPromptReady {
  [[ "${options[prompt_subst]}" == "on" ]] &&
    [[ -n "${fg[red]-}" ]] &&
    (( $+functions[git_prompt_info] )) &&
    (( $+functions[git_prompt_status] ))
}

function applyAntigenConfig {
  local antigen_path="$1"

  source "$antigen_path" || return 1

  antigen use oh-my-zsh || return 1

  antigen bundles <<EOBUNDLES || return 1
  command-not-found
  nvm
  rupa/z
  zsh-users/zsh-autosuggestions
  zsh-users/zsh-syntax-highlighting
EOBUNDLES

  antigen theme kphoen || return 1

  antigen apply || return 1
}

function initAntigen {
  local antigen_path="$1"
  local adotdir="${ADOTDIR:-$HOME/.antigen}"
  local lock_file="$adotdir/init.lock"
  local lock_fd

  if [ ! -f "$antigen_path" ]; then
    >&2 echo "Failed to init antigen, not found at: $antigen_path"
    fallbackPrompt
    return 1
  fi

  mkdir -p "$adotdir"
  touch "$lock_file" 2>/dev/null || {
    >&2 echo "Failed to create Antigen init lock at: $lock_file"
    fallbackPrompt
    return 1
  }

  zmodload zsh/system 2>/dev/null || {
    >&2 echo "Failed to load zsh/system for Antigen locking"
    fallbackPrompt
    return 1
  }

  if ! zsystem flock -f lock_fd -t 60 "$lock_file"; then
    >&2 echo "Timed out waiting for Antigen initialization lock"
    fallbackPrompt
    return 1
  fi

  applyAntigenConfig "$antigen_path"
  if ! antigenPromptReady; then
    clearAntigenGeneratedState
    applyAntigenConfig "$antigen_path"
  fi

  zsystem flock -u "$lock_fd"

  if ! antigenPromptReady; then
    >&2 echo "Failed to initialize Antigen prompt; using fallback prompt"
    fallbackPrompt
    return 1
  fi
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
