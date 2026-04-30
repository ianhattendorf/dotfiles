export XZ_DEFAULTS="--threads=0"
#export KUBECONFIG=~/.kube/sandbox.conf
#export KUBECONFIG=~/.kube/minikube.conf
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:=$HOME/.config}"
if [ -r "$XDG_CONFIG_HOME/dotfiles/session-env.sh" ]; then
  . "$XDG_CONFIG_HOME/dotfiles/session-env.sh"
fi
if [ -z "${SSH_AUTH_SOCK:-}" ] && [ -n "${XDG_RUNTIME_DIR:-}" ]; then
  export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
fi
#export GK_OUT_DIR_WITH_SPACES=1
if [ -f "$HOME/.cargo/env" ]; then
  . "$HOME/.cargo/env"
fi
# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$HOME/go/bin:$BUN_INSTALL/bin:$PATH"
