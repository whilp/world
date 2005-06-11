# aliases
if [ "$TERM" != "dumb" ]; then
    eval `dircolors -b`
    alias ls='ls --color=auto'
fi
alias myterm='Eterm --trans --scrollbar false --buttonbar false -c yellow -f rgb:0/ffff/0 --cmod 40 -g 140x24 -v'
alias top='top'
alias mutt='mutt'
alias xlockb='xlock -mode blank'
alias s='screen'

export LANG="C"

PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/X11R6/bin:/opt/:/usr/games/:/home/will/bin

if [ -z "$debian_chroot" -a -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi
case "$TERM" in (Eterm|xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"'
    ;;
*)
    ;;
esac
SHELL=/usr/bin/zsh
