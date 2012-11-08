export SHELL=zsh
setopt SH_WORD_SPLIT
. ~/.profile
unsetopt SH_WORD_SPLIT

bindkey -v

case $TERM in
    xterm*) bindkey "^[[F" end-of-line; bindkey "^[[H" beginning-of-line;;
esac

bindkey -M vicmd v edit-command-line
bindkey '\e[1~' beginning-of-line
bindkey '\e[4~' end-of-line
bindkey '\e[3~' delete-char
bindkey '\e[2~' overwrite-mode
bindkey "^[[A"  up-line-or-search
bindkey "^[[B"  down-line-or-search
bindkey -v

stty erase  2>/dev/null

autoload -U compinit && compinit -i
autoload -U colors && colors
autoload -U edit-command-line
autoload -U promptinit && promptinit
autoload -U zed
autoload -U zmv
autoload edit-command-line
zle -N edit-command-line
zmodload -i zsh/complist

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

zstyle ':completion:*:descriptions' format "- %d -"
zstyle ':completion:*:corrections' format "- %d - (errors %e})"
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*' menu select
zstyle ':completion:*' verbose yes

alias mv='nocorrect mv'
alias man='nocorrect man'
alias cp='nocorrect cp'
alias mkdir='nocorrect mkdir'
alias cvs='nocorrect cvs'
alias ln='nocorrect ln'

PS1=$'%{\e\a%}%B%~%b %# '

setopt NO_beep
setopt NO_check_jobs
setopt NO_hup
setopt NO_nullglob
setopt NO_singlelinezle
setopt always_last_prompt
setopt alwaystoend
setopt auto_cd
setopt autolist
setopt autopushd
setopt bsd_echo
setopt complete_aliases
setopt complete_in_word
setopt completeinword
setopt correct
setopt extended_glob
setopt globdots
setopt histignoredups
setopt histverify
setopt listpacked
setopt nobeep
setopt noclobber
setopt nohup
setopt nolisttypes
setopt notify
setopt path_dirs
setopt rcquotes
setopt shwordsplit

export ZSHDIR=$HOME/.zsh
HISTSIZE=1000
SAVEHIST=1000
if [ $(id -u) -eq 0 ]; then
    HISTFILE=/root/.history
else
    HISTFILE=$HOME/.history
fi
INC_APPEND_HISTORY=1
SHARE_HISTORY=1
HIST_IGNORE_ALL_DUPS=1
HIST_IGNORE_SPACE=1
NO_HIST_BEEP=1
