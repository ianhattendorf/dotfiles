if [ -n "${XDG_RUNTIME_DIR:-}" ]; then
  export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-tpm-agent.sock"
fi
