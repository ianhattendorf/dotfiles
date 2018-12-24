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
  exec startx
elif [[ ! $DISPLAY && $XDG_VTNR -eq 2 ]]; then
  exec sway
fi

