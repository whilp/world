##################  BEGIN HEADERS
# Filename	: $HOME/.zshrc
# Use		: setup file for zsh (z shell)
# Author	: Will Maier <willmaier@ml1.net>
# Vim           : vim: set ft=sh:
# CVS		: $Id: zshrc,v 1.60 2006/07/18 14:54:25 will Exp $
# Copyright	: Copyright (c) 2005-2006 Will Maier
# License	: BSD; see <http://www.lfod.us/copyright.html>
##################  END HEADERS

# Grab general settings
[ -r "${HOME}/.profile" ] && . "${HOME}/.profile"

# Terminal settings
bindkey -v

# Make HOME and END work reasonably
case $TERM in
    xterm*)
	bindkey "^[[F" end-of-line
	bindkey "^[[H" beginning-of-line
	;;
esac

bindkey '\e[1~' beginning-of-line       # Home
bindkey '\e[4~' end-of-line             # End
bindkey '\e[3~' delete-char             # Del
bindkey '\e[2~' overwrite-mode          # Insert
bindkey "^[[A"  up-line-or-search       # cursor up
bindkey "^[[B"  down-line-or-search     # <ESC>-
bindkey -v

# Set BACKSPACE
# stty erase '^H'
stty erase  2>/dev/null

# Load personal function stuff
fpath=(~/.zsh $fpath)

# Load tricksy zsh functions
autoload -U compinit && compinit -i # new tab completion
autoload -U colors && colors	    # color stuff
autoload -U edit-command-line
autoload -U zed			    # shell text editing
autoload -U zmv			    # a la mmv/rename
autoload      edit-command-line     # press ESC-E to edit the comand
zle -N        edit-command-line     # line in vi(1)
bindkey '\ee' edit-command-line
zmodload -i zsh/complist	    # for completion

# Cache function results for compsys
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

# Completion customizations, adding nice menus, descriptions, etc
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX + $#SUFFIX) / 3 )) )'
zstyle ':completion:*:descriptions' format "- %d -"
zstyle ':completion:*:corrections' format "- %d - (errors %e})"
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*' menu select
zstyle ':completion:*' verbose yes

# Use ~/.ssh/ to determine hostnames
[ -f ~/.ssh/config ] && : ${(A)ssh_config_hosts:=${${${${(@M)${(f)"$(<~/.ssh/config)"}:#Host *}#Host }:#*\**}:#*\?*}}
[ -f ~/.ssh/known_hosts ] && : ${(A)ssh_known_hosts:=${${${(f)"$(<$HOME/.ssh/known_hosts)"}%%\ *}%%,*}}
zstyle ':completion:*:*:*' hosts $ssh_config_hosts $ssh_known_hosts

watch=(notme root)		    # Note logins

# zsh aliases
alias mv='nocorrect mv'		    # no spelling correction on mv
alias man='nocorrect man'	    # ...
alias cp='nocorrect cp'		    
alias mkdir='nocorrect mkdir'	    
alias cvs='nocorrect cvs'
alias ln='nocorrect ln'
alias zource=". ${HOME}/.zshrc"

# Shell prompt
if [ "${EUID}" != "0" ]; then
    # If not root...
    autoload -U promptinit && promptinit
    PS1="%B%~%b %#%b "		    # real prompt
else
    PS1="%B%~%b %#%b "
fi

# Set the right prompt based on the screen sessionname and window number, or,
# if not running in screen, the (pseudo) TTY.
if [ -n "${STY}" ]; then
    # GNU screen sets $STY; if it's non-zero, assume we're in screen.

    # Determine the name of the current screen session from $STY
    SESSIONNAME="${STY##*.}"
    if [ "${SESSIONNAME}" = "${HOSTNAME}" ]; then
	SESSIONNAME="screen"
    fi

    NAME=${SESSIONNAME}
    NUMBER=${WINDOW}
else
    # We're not in screen; let's figure out what TTY we're on instead.

    TTYOUT="$(tty)"
    TTYNODEV="${TTYOUT##*/}"
    TTYNAME="${TTYNODEV%?}"
    TTYINC="$(echo "${TTYNODEV}" | sed -e "s/${TTYNAME}//" 2>/dev/null)"

    if [ "${#TTYNAME}" -lt "1" ]; then
	# If it doesn't work...the above seems fine for FreeBSD; Linux
	# prefers the following.
	TTYNODEV="${TTYOUT#*/*/}"
	TTYNAME="${TTYNODEV%/*}"
	TTYINC="${TTYNODEV##*/}"
    fi

    NAME=${TTYNAME}
    NUMBER=${TTYINC}
fi
# Assemble the right prompt
RPS1="%B ${NAME}[${NUMBER}] @ ${HOSTNAME} %(0?,,E[%?])%b"

xtitle () {
    MESSAGE="$1"
    print -Pn "\e]0;${HOSTNAME}[${MESSAGE}]\a"
}
ns () {
    if [ "$(env | grep -E '(X|COLOR)TERM')" ]; then
        UTITLE=$(echo $1 | tr '[:lower:]' '[:upper:]')
        LTITLE=$(echo $1 | tr '[:upper:]' '[:lower:]')
        print -Pn "\e]0;${HOSTNAME}[${UTITLE}]\a"
    fi
    screen -S "${LTITLE}"
}

# Set zsh options
setopt NO_beep
setopt NO_check_jobs	    # don't notify re: jobs when shell exits
setopt NO_hup
setopt NO_nullglob
setopt NO_singlelinezle
setopt always_last_prompt   # req'd by menu selection
setopt alwaystoend	    # move cursor to end of word when completing
setopt auto_cd		    # zsh adds 'cd ' when you enter a dir name
setopt autopushd	    # type ~-<Tab> for dirs you've been to
setopt autolist
setopt bsd_echo
setopt complete_aliases
setopt completeinword	    # internal word completion
setopt correct		    # try to correct first word spelling
#setopt correct_all	    # correct all words
setopt complete_in_word	    # complete even within words. nice.
setopt extended_glob
setopt globdots		    # include dotfiles in globs
setopt histignoredups	    # don't duplicate entires in command history
setopt histverify	    # prompt when using history commands
setopt listpacked	    # compact completion lists
setopt nobeep
setopt noclobber	    # don't overwrite files on redirect
setopt nohup		    # don't kill jobs when shell exits
setopt nolisttypes	    # show types in completion
setopt notify
setopt shwordsplit          # Expand variables for iteration like sh
setopt path_dirs
#setopt pushignoredups	    # don't duplicate entries in dir history
setopt rcquotes		    # elegant quoting of quotes ('"' -> ')

# Set variables that zsh cares about
export ZSHDIR=$HOME/.zsh
export VISUAL='vim'
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
LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;34;01:ex=01;32:*.tar=01;34:*.tgz=01;34:*.gz=01;34:*.bz2=01;34:*.jpg=01;35:*.sh=103;34:*.ogg=01;31:*.mp3=01;31:*.avi=01;34:*.mpg=01;34:*.mpeg=01;34:*.wmv=01;34:*.asf=01;34'
