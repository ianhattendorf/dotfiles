function initAntigen {
  if [ ! -f "$1" ]; then
    >&2 echo "Failed to init antigen, not found at: $1"
    return 1
  fi

  source "$1"

  antigen use oh-my-zsh

  antigen bundles <<EOBUNDLES
  brew
  command-not-found
  nvm
  rupa/z
  zsh-users/zsh-autosuggestions
  zsh-users/zsh-syntax-highlighting
EOBUNDLES

  antigen theme kphoen

  antigen apply
}

function macOsPathSetup {
  if [ -x /usr/libexec/path_helper ]; then
    eval `/usr/libexec/path_helper -s`
    env_plist=$HOME/Library/LaunchAgents/environment.plist
    if [ ! -f $env_plist ]
    then
      # NOTE: doesn't seem to be working. Instead, can run `sudo launchctl config user path $PATH`
      # See: https://github.com/ersiner/osx-env-sync/issues/1
      /bin/cat <<EOM >> $env_plist
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>Label</key>
	<string>my.startup</string>
	<key>ProgramArguments</key>
	<array>
		<string>sh</string>
		<string>-c</string>
		<string>launchctl setenv PATH /usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin</string>
	</array>
	<key>RunAtLoad</key>
	<true/>
</dict>
</plist>
EOM
    fi
    /usr/libexec/PlistBuddy -c "Set :ProgramArguments:2 launchctl setenv PATH $PATH" $env_plist
  fi
}

function initOs {
  case `uname` in
    Darwin)
      macOsPathSetup

      export SSH_AUTH_SOCK=$HOME/.gnupg/S.gpg-agent.ssh
      gpg-connect-agent updatestartuptty /bye > /dev/null

      initAntigen /usr/local/share/antigen/antigen.zsh
      ;;
    Linux)
      export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh"

      initAntigen ~/bin/antigen-2.2.3.zsh

      if [ ! $DISPLAY ]; then
        # Start Sway on tty1, i3 on tty2
        if [[ $XDG_VTNR -eq 1 ]]; then
          exec sway
        elif [[ $XDG_VTNR -eq 2 ]]; then
          exec startx
        fi
      fi
      ;;
  esac
}

function initPath {
  if [ -f ~/.cargo/env ]; then
    . ~/.cargo/env
  fi
}

function initMisc {
  if [ -d $HOME/.nvm ]; then
    export NVM_DIR="$HOME/.nvm"
  fi

  if [ -f /usr/local/opt/nvm/nvm.sh ]; then
    . /usr/local/opt/nvm/nvm.sh
  fi
}

function init {
  initOs
  initPath
  initMisc
}

init
