# .profile

export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh

# User specific environment and startup programs

PATH=$PATH:$HOME/.local/bin:$HOME/bin
PATH="$HOME/.cargo/bin:$PATH"
export PATH