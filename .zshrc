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
  autoload -U +X bashcompinit && bashcompinit
  if [ -f /usr/bin/terraform ]; then
    complete -o nospace -C /usr/bin/terraform terraform
  fi
}

function initOs {
  case `uname` in
    Linux)
      initAntigen ~/bin/antigen-2.2.3.zsh
      initAutocomplete
      if [ ! "$DISPLAY" ]; then
        # Start Sway on tty1 or tty4, i3 on tty5
        if [ "$XDG_VTNR" -eq 1 ] || [ "$XDG_VTNR" -eq 4 ]; then
          exec start-sway
        elif [ "$XDG_VTNR" -eq 5 ]; then
          exec startx
        fi
      fi
      ;;
  esac
}

function init {
  initOs
}

init
