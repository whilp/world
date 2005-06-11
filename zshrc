# Will's .zshrc; started 2005.03.18 <willmaier@ml1.net>

EDITOR=vim

source ~/.profile
# command history
HISTSIZE=1000
SAVEHIST=1000
HISTFILE='/home/will/.history'
INC_APPEND_HISTORY=1
SHARE_HISTORY=1
HIST_IGNORE_ALL_DUPS=1
HIST_IGNORE_SPACE=1
NO_HIST_BEEP=1

# prompt
PS1='<%B%m%b %T> %~ %# '
case $TERM in
    xterm*|Eterm*|screen)
	precmd () {print -Pn "\e]0;%n@%m: %~\a"}
	;;
esac

# ensure home/end/del/ins work as expected
    bindkey '\e[1~' beginning-of-line	# home
    bindkey '\e[4~' end-of-line		# end
    bindkey '\e[3~' delete-char		# del
    bindkey '\e[2~' overwrite-mode	# ins

case $TERM in 
    screen|*xterm*|*rxvt*|(a|dt|k)term)
    precmd () { print -Pn "\e]0;$USER@$HOST - %~\a" }
    preexec () { print -Pn "\e]0;$USER@$HOST- $1\a" }
    ;;
esac
# screen/title hackery
#function title {
#if [[ $TERM == "screen" ]]; then
#    # Use these two for GNU Screen:
#    print -nR $\u2019 33k\u2019$1$\u2019 33\u2019\
#    print -nR $\u2019 33]0;\u2019$2$\u2019\u2019
#elif [[ $TERM == "xterm" || $TERM == "aterm" ]]; then
#    # Use this one instead for XTerms:
#    print -nR $\u2019
#    33]0;\u2019$*$\u2019\u2019
#fi
#}
#
#function precmd {
#    title zsh "$PWD"
#}
#
#function preexec {
#    emulate -L zsh
#    local -a cmd; cmd=(${(z)1})
#    title $cmd[1]:t "$cmd[2,-1]"
#}
