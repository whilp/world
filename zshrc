# ~/.zshrc
# Will Maier <willmaier@ml1.net>
# 2005.03.18

source ~/.profile

# --[ ENVIRONMENT
    HISTSIZE=1000
    SAVEHIST=1000
    HISTFILE=$HOME/.history
    INC_APPEND_HISTORY=1
    SHARE_HISTORY=1
    HIST_IGNORE_ALL_DUPS=1
    HIST_IGNORE_SPACE=1
    NO_HIST_BEEP=1
    # Fix keys
    bindkey '\e[1~' beginning-of-line	# home
    bindkey '\e[4~' end-of-line		# end
    bindkey '\e[3~' delete-char		# del
    bindkey '\e[2~' overwrite-mode	# ins
    stty erase '^?'
    # Ensure tab completion works
    autoload -U compinit
compinit -C
    
# --[ PROMPT
    PS1='<%B%m%b %T> %~ %# '
    case $TERM in
	xterm*|Eterm*|screen)
	precmd () { print -Pn "\e]0;$HOST - %~\a" }
	preexec () { print -Pn "\e]0;$HOST - $1\a" }
	;;
    esac
