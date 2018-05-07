source ~/bin/antigen-2.2.3.zsh

antigen use oh-my-zsh

antigen bundles <<EOBUNDLES
  command-not-found
  rupa/z
  zsh-users/zsh-autosuggestions
  zsh-users/zsh-syntax-highlighting
EOBUNDLES

antigen theme bira

antigen apply

# Start WM if on tty1
if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  short_hostname=`hostname --short`
  if [ "$short_hostname" = 'nala' ]; then
    # Default to i3/X11 until Sway v1.0
    exec startx
  else
    exec startx
  fi
fi

