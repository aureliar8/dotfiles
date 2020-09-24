alias o='xdg-open'
alias c='clear'
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

if command -v exa &>/dev/null
then
	alias ls='exa -a --color=always --group-directories-first' # my preferred listing
	alias ll='exa -l --color=always --group-directories-first'  # long format
	alias lt='exa -aT --color=always --group-directories-first' # tree listing
fi

alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

