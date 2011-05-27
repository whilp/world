WHICH=$(/usr/bin/env which which)
# Environment variables
EDITOR="$($WHICH vim 2>/dev/null)" || \
    EDITOR="$($WHICH nvi)" || \
    EDITOR="$($WHICH vi)"
VISUAL=${EDITOR}
ENV="~/.kshrc"
SHELL_OLD="${SHELL}"
SHELL="$($WHICH mksh 2>/dev/null)" || \
    SHELL="$($WHICH ksh 2>/dev/null)" || \
    SHELL="$($WHICH zsh 2>/dev/null)" || \
    SHELL="$($WHICH bash 2>/dev/null)" || \
    SHELL="$($WHICH sh)"
HISTFILE=~/.history
HOSTNAME="$(hostname -s)"
LANG="C"
LESSHISTFILE=
OLDMAIL="${MAIL}"
MAIL=""
MAXPS1LEN=30
PAGER="less -iX"
PATH="$HOME/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games"
SSH="$(which ssh)"
TMUX_SOCK=~/.tmux/sock

export EDITOR ENV HISTFILE HOSTNAME LANG LESSHISTFILE 
export MAIL MAXPS1LEN OLDMAIL PATH PAGER SHELL TMUX_SOCK
unset WHICH

# CVS.
CVSEDITOR="${EDITOR}"
CVS_RSH=/usr/bin/ssh
OCVS=:ext:cvs:/cvs

export CVSEDITOR CVS_RSH OCVS

# Miscellaneous.
BBCURL=mms://a279.l3944048278.c39440.g.lm.akamaistream.net/D/279/39440/v0001/reflector:48278

export BBCURL

# Functions.
addto () {
    typeset STRING=$1
    typeset NEW=$2
    typeset AFTER=$3
    case "${STRING}" in 
        ${NEW}|${NEW}:*|*:${NEW}|*:${NEW}:*);;
        *) [ "${AFTER}" = "after" ] && STRING="${STRING}:${NEW}" || STRING="${NEW}:${STRING}" ;; 
    esac
    echo ${STRING}
}
sleepuntil () {
    typeset DATE=$1 
    typeset INTERVAL=${2:-60} 
    typeset TARGET=$(date -j "${DATE}" "+%s" 2>/dev/null) 
    if [ -z "${TARGET}" ]; then
        echo "bad date '$DATE'"
        return 1
    fi
    echo "Sleeping until $(date -j "${DATE}")..."
    while [ "$(date "+%s")" -lt "${TARGET}" ]
    do
        sleep "${INTERVAL}"
    done
}
agent () {
    [ -r "${HOME}"/bin/agent ] && . "${HOME}"/bin/agent
}
site () {
    if [ -z "$1" ]; then
        echo "${SITE}"
    else
        SITE=$1; export SITE
    fi
}

# Aliases.
alias curl="curl -s"
alias dc="cd"
alias elinks="DISPLAY='' elinks -touch-files -no-connect"
alias h="todo -f ~/share/todo/hep.txt -p hep"
alias hep="site hep"
alias less="${PAGER}"
alias lfod="site lfod"
alias list="tmux ls"
alias ls="ls -F"
alias mtr="mtr -t"
alias sudo='A=`alias` /usr/bin/sudo '
alias t="todo -f ~/.todo -p todo"
alias tmux="tmux -S ${TMUX_SOCK}"
alias vi="${VISUAL}"
alias vimdiff="vimdiff -o"
alias xinit="xinit -- -nolisten tcp"

# Platform- and host-specific configuration directories.
PROFILES="${HOME}/.profiles"
PLATFORMS="${PROFILES}/platforms"
HOSTS="${PROFILES}/hosts"

# Platform settings.
UNAME=$(uname)
case "${UNAME}" in
    OpenBSD)    PLATFORM="${PLATFORMS}/openbsd";;
    Linux)      PLATFORM="${PLATFORMS}/linux";;
esac
[ -r "${PLATFORM}" ] && . "${PLATFORM}"

# Domain and host settings.
FQDN=.$(hostname)
f=${FQDN}
max=5
while [ -n "$f" -a $max -ge 0 ]; do
    f=${f%.*}
    SETTINGS="${HOSTS}/${FQDN#$f.}"
    [ -r "${SETTINGS}" ] && . "${SETTINGS}"
    max=$(($max - 1))
done
unset f max PLATFORM SETTINGS
    
# Virtualenv.
OLDPS1="${PS1}"
if [ -r "${HOME}/bin/activate" ]; then
    . "${HOME}/bin/activate"
fi
export PS1="${OLDPS1}"

agent
ulimit -n 1023

# Run the preferred shell (unless we're already running it).
if [ "${SHELL##*/}" != "${SHELL_OLD##*/}" ]; then
    eval "exec ${SHELL} -l"
fi
