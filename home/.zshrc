function fallbackPrompt {
  autoload -U colors && colors
  setopt prompt_subst
  PROMPT='[%{$fg[red]%}%n%{$reset_color%}@%{$fg[magenta]%}%m%{$reset_color%}:%{$fg[blue]%}%~%{$reset_color%}]
%# '
  RPROMPT=''
}

function zshPromptReady {
  [[ "${options[prompt_subst]}" == "on" ]] &&
    [[ -n "${fg[red]-}" ]] &&
    (( $+functions[git_prompt_info] )) &&
    (( $+functions[git_prompt_status] ))
}

function loadAntidotePlugins {
  local manifest="$1"
  local generated="$2"
  local generated_tmp="${generated}.$$"

  if [ ! -f "$manifest" ]; then
    >&2 echo "Failed to init Antidote, plugin manifest not found at: $manifest"
    fallbackPrompt
    return 1
  fi

  if [ ! -f "$generated" ] || [ "$manifest" -nt "$generated" ]; then
    antidote bundle < "$manifest" >| "$generated_tmp" && mv -f "$generated_tmp" "$generated" || {
      rm -f "$generated_tmp"
      >&2 echo "Failed to generate Antidote plugin bundle from: $manifest"
      fallbackPrompt
      return 1
    }
  fi

  source "$generated" || {
    >&2 echo "Failed to source Antidote plugin bundle at: $generated"
    fallbackPrompt
    return 1
  }
}

function initAntidote {
  local antidote_path="${XDG_DATA_HOME:-$HOME/.local/share}/antidote/antidote.zsh"
  local zcache="${XDG_CACHE_HOME:-$HOME/.cache}/antidote"
  local zdotdir="${ZDOTDIR:-$HOME}"

  if [ ! -f "$antidote_path" ]; then
    >&2 echo "Failed to init Antidote, not found at: $antidote_path"
    fallbackPrompt
    return 1
  fi

  mkdir -p "$zcache" || {
    >&2 echo "Failed to create Antidote cache dir at: $zcache"
    fallbackPrompt
    return 1
  }

  export ANTIDOTE_HOME="$zcache"
  source "$antidote_path" || {
    >&2 echo "Failed to source Antidote at: $antidote_path"
    fallbackPrompt
    return 1
  }
  loadAntidotePlugins "$zdotdir/.zsh_plugins.txt" "$zcache/zsh_plugins.zsh"
}

function initAntidoteAfterCompinit {
  local zcache="${XDG_CACHE_HOME:-$HOME/.cache}/antidote"
  local zdotdir="${ZDOTDIR:-$HOME}"

  loadAntidotePlugins "$zdotdir/.zsh_plugins_after_compinit.txt" "$zcache/zsh_plugins_after_compinit.zsh"
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
  local antidote_status

  case `uname` in
    Linux)
      initAntidote
      antidote_status=$?
      initAutocomplete
      if [ "$antidote_status" -eq 0 ]; then
        initAntidoteAfterCompinit
      fi
      if [ "$antidote_status" -eq 0 ] && ! zshPromptReady; then
        >&2 echo "Failed to initialize Zsh prompt; using fallback prompt"
        fallbackPrompt
      fi
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
