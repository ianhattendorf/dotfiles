if set -q XDG_RUNTIME_DIR
	# Set SSH_AUTH_SOCK
	set -gx SSH_AUTH_SOCK $XDG_RUNTIME_DIR/ssh-agent.socket
end

set -gx DOTNET_ROOT $HOME/.dotnet
fish_add_path $HOME/.dotnet
