# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/cygdrive/c/Users/asiegler/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

PROMPT='%m:%~%# '
RPROMPT='[%*]'

EDITOR='emacs -nw'

alias ls='ls -F --color=auto'
alias ll='ls -l'
alias la='ls -a'

bindkey '^W' vi-backward-kill-word

