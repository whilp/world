##################  BEGIN HEADERS
# Filename	: $HOME/.zshrc
# Use		: setup file for zsh (z shell)
# Author	: Will Maier <willmaier@ml1.net>
# Updated	: 2005.10.04 12:55:39 -0500
##################  END HEADERS

source ~/.profile

# --[ LOAD FUNCTIONS
autoload -U compinit && compinit    # new tab completion
autoload -U colors && colors	    # color stuff
autoload -U edit-command-line
autoload -U zed			    # shell text editing
autoload -U zmv			    # a la mmv/rename

zmodload -i zsh/complist	    # for completion

# --[ SET ALIASES
alias -s tex=$EDITOR		    # eg 'unixbook.tex<CR>' opens unixbook in
				    # vim
alias -s html='elinks'		    # html -> www browser
alias mv='nocorrect mv'		    # no spelling correction on mv
alias cp='nocorrect cp'		    # no spelling correction on cp
alias mkdir='nocorrect mkdir'	    # no spelling correction on mkdir
alias grep=egrep

# --[ TERM MAGIC
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
stty erase '^?'

# --[ PROMPT
if (( EUID != 0 )); then
    # If not root...
    autoload -U promptinit && promptinit
#    PS1='<%B%m%b %T> %~ %# '
    PS1="%B%~%b %#%b "
#    precmd () { print -Pn "\e]0;$HOST - %~\a" }
#    preexec () { echo "OK" }
else
    PS1='<%B%m%b %T> %~ %# '
    PS1="%B%~%b %#%b "
fi
HOSTNAME=$(hostname -s)
if [[ "$HOSTNAME" == "localhost" ]]; then
    HOSTNAME=$(hostname)
fi
SCREEN="$(echo $STY | sed 's/.*\.\(.*\)/\1/')"
if [[ "$SCREEN" == "$HOSTNAME" ]]; then
    SCREEN="screen"
fi
if [[ -z "$WINDOW" ]]; then
    WINDOW=$(tty | sed 's/.*\(tty.*\)/\1/')
else
    WINDOW=[${WINDOW}]
fi
RPS1="%B ${WINDOW:+${SCREEN}$WINDOW on} ${HOSTNAME} %(0?,,E[%?])%b"

# --[ IMPORTANT VARIABLES
export ZSHDIR=$HOME/.zsh
export VISUAL='vim'
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=$HOME/.history
INC_APPEND_HISTORY=1
SHARE_HISTORY=1
HIST_IGNORE_ALL_DUPS=1
HIST_IGNORE_SPACE=1
NO_HIST_BEEP=1
LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;34;01:ex=01;32:*.tar=01;34:*.tgz=01;34:*.gz=01;34:*.bz2=01;34:*.jpg=01;35:*.sh=103;34:*.ogg=01;31:*.mp3=01;31:*.avi=01;34:*.mpg=01;34:*.mpeg=01;34:*.wmv=01;34:*.asf=01;34'

# --[ OPTIONS
setopt NO_beep
setopt NO_check_jobs	    # don't notify re: jobs when shell exits
setopt NO_hup
setopt NO_nullglob
setopt NO_singlelinezle
setopt always_last_prompt   # req'd by menu selection
setopt alwaystoend	    # move cursor to end of word when completing
setopt auto_cd		    # zsh adds 'cd ' when you enter a dir name
setopt autolist
setopt bsd_echo
setopt complete_aliases
setopt completeinword	    # internal word completion
setopt correct		    # try to correct first word spelling
setopt correct_all	    # correct all words
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
setopt path_dirs
#setopt pushignoredups	    # don't duplicate entries in dir history
setopt rcquotes		    # elegant quoting of quotes ('"' -> ')

# --[ ENVIRONMENT
