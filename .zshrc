# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

#  Dependancies You Need for this Config
# zsh-syntax-highlighting - syntax highlighting for ZSH in standard repos
# autojump - jump to directories with j or jc for child or jo to open in file manager
# zsh-autosuggestions - Suggestions based on your history
# Enable colors 
autoload -U colors && colors


HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)               # Include hidden files.

autoload -U bashcompinit
bashcompinit

#source /usr/local/etc/bash_completion.d/*

# Custom ZSH Binds
bindkey '^ ' autosuggest-accept

[ -f "$HOME/.bash_aliases" ] && source "$HOME/.bash_aliases"


M2_HOME=/opt/apache-maven-3.6.3
ANT_HOME=/opt/apache-ant-1.10.8


export PATH=$PATH:$M2_HOME/bin
export PATH=$PATH:$ANT_HOME/bin

export JAVA_HOME='/Library/Java/JavaVirtualMachines/jdk1.8.0_201.jdk/Contents/Home/'
alias tomcat='/opt/apache-tomcat-8.0.28/bin/catalina.sh' 
# For some reason emacs do not work
alias emacs=/Applications/Emacs.app/Contents/MacOS/Emacs
export EDITOR=/Applications/Emacs.app/Contents/MacOS/Emacs

export PATH=$PATH:$HOME/go/bin/


source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh 2>/dev/null
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
POWERLINE_HOME=/opt/powerlevel10k/
source $POWERLINE_HOME/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
