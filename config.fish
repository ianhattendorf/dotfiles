if test -e /etc/fedora-release
	# Set SSH_AUTH_SOCK
	set -gx SSH_AUTH_SOCK $XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh

	# Set GPG TTY
#	set -gx GPG_TTY (tty)

	# Refresh gpg-agent tty in case user switches into an X session
#	gpg-connect-agent updatestartuptty /bye >/dev/null
else
	set -x SSH_AUTH_SOCK $XDG_RUNTIME_DIR/ssh-agent.socket
end
