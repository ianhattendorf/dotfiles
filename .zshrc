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

function initOs {
  case `uname` in
    Linux)
      initAntigen ~/bin/antigen-2.2.3.zsh
      # TODO is this still necessary? maybe only with sway?
      # gpg-connect-agent updatestartuptty /bye

      if [ ! $DISPLAY ]; then
        # Start Sway on tty4, i3 on tty5
        if [[ $XDG_VTNR -eq 4 ]]; then
          export XDG_SESSION_TYPE=wayland
          export QT_QPA_PLATFORM=wayland
          export QT_WAYLAND_DISABLE_WINDOWDECORATION=1

          exec dbus-launch --sh-syntax --exit-with-session sway > ~/.sway.log 2> ~/.sway.error.log
        elif [[ $XDG_VTNR -eq 5 ]]; then
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
