export PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\w\[\033[m\]\$ "
export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad
alias ls='ls -GFh'

# Script for ensuring only one instance of gpg-agent is running
# and if there is not one, start an instance of gpg-agent.
if test -f $HOME/.gpg-agent-info && kill -0 `cut -d: -f 2 $HOME/.gpg-agent-info` 2>/dev/null; then
	GPG_AGENT_INFO=`cat $HOME/.gpg-agent-info`
	export GPG_AGENT_INFO SSH_AUTH_SOCK SSH_AGENT_PID
else
	eval `gpg-agent --daemon`
	echo $GPG_AGENT_INFO >$HOME/.gpg-agent-info
fi
# Imperative that this environment variable always reflects the output
# of the tty command.
GPG_TTY=`tty`
export GPG_TTY

# Homebrew
export 	HOMEBREW_GITHUB_API_TOKEN=e8457229b1f267b0576379f8e69f9d8cf30705da
source `brew --repository`/Library/Contributions/brew_bash_completion.sh

# rbenv
eval "$(rbenv init -)"

# bash completion
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

# aliases
alias bu='bundle'
alias be='bundle exec'
