if test -e /etc/fedora-release
	set -gx SSH_AUTH_SOCK (gnome-keyring-daemon --start | grep "^SSH_AUTH_SOCK" | awk -F "=" '{print $2}')
else
	set -x SSH_AUTH_SOCK $XDG_RUNTIME_DIR/ssh-agent.socket
end
