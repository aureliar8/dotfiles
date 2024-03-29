# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

#  Dependancies You Need for this Config
# zsh-syntax-highlighting - syntax highlighting for ZSH in standard repos
# zsh-autosuggestions - Suggestions based on your history
# Enable colors 
autoload -U colors && colors

export HISTFILE=$HOME/.zhistory
export HISTSIZE=1000000000
export SAVEHIST=$HISTSIZE
setopt EXTENDED_HISTORY          # Write the history file in the ":start:elapsed;command" format.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)               # Include hidden files.

autoload -U bashcompinit
bashcompinit

export TERM=xterm-256color

#source /usr/local/etc/bash_completion.d/*
# Custom ZSH Binds
bindkey '^ ' autosuggest-accept

[ -f "$HOME/.bash_aliases" ] && source "$HOME/.bash_aliases"

source /home/aurelien/bin/zsh-fzf-history-search.zsh 
source /opt/zsh-autosuggestions/zsh-autosuggestions.zsh
source /opt/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
POWERLINE_HOME=/opt/powerlevel10k
source $POWERLINE_HOME/powerlevel10k.zsh-theme
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

autoload -U +X bashcompinit && bashcompinit

PATH=$PATH:$HOME/scripts

complete -o nospace -C /usr/bin/terraform terraform
export LIBVIRT_DEFAULT_URI="qemu:///system" 

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

