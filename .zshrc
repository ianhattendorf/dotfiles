source ~/antigen.zsh

antigen use oh-my-zsh

antigen bundles <<EOBUNDLES
  command-not-found
  rupa/z
  zsh-users/zsh-autosuggestions
  zsh-users/zsh-syntax-highlighting
EOBUNDLES

antigen theme bira

antigen apply
