# .bashrc

# Source ~/.profile
if [ -f ~/.profile ]; then
	. ~/.profile
fi

# Source /etc/bashrc
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
. "$HOME/.cargo/env"
