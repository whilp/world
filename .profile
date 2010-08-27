WHICH=$(/usr/bin/env which which)
# Environment variables
ENV="~/.kshrc"
EDITOR="$($WHICH nvi 2>/dev/null)" || \
    EDITOR="$($WHICH vi)" || \
    EDITOR="$($WHICH vim)"
VISUAL=${EDITOR}
SHELL="/bin/ksh"
HOSTNAME="$(hostname -s)"
LANG="C"
OLDMAIL="${MAIL}"
MAIL=""
PAGER="less -iX"
PATH="$HOME/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games"
SSH="$(which ssh)"
TMUX_SOCK=~/.tmux/sock

export EDITOR HOSTNAME LANG MAIL PATH PAGER SHELL TODO TMUX_SOCK

# CVS.
CVSEDITOR="${EDITOR}"
CVS_RSH=/usr/bin/ssh
OCVS=:ext:cvs:/cvs

export CVSEDITOR CVS_RSH OCVS

# Platform- and host-specific configuration directories.
PROFILES="${HOME}/.profiles"
PLATFORMS="${PROFILES}/platforms"
HOSTS="${PROFILES}/hosts"

# Functions.
addto () {
    STRING=$1
    NEW=$2
    AFTER=$3
    case "${STRING}" in 
        ${NEW}|${NEW}:*|*:${NEW}|*:${NEW}:*);;
        *) [ "${AFTER}" = "after" ] && STRING="${STRING}:${NEW}" || STRING="${NEW}:${STRING}" ;; 
    esac
    echo ${STRING}
}
sleepuntil () {
    DATE=$1 
    INTERVAL=${2:-60} 
    TARGET=$(date -j "${DATE}" "+%s") 
    echo "Sleeping until $(date -j "${DATE}")..."
    while [ "$(date "+%s")" -lt "${TARGET}" ]
    do
        sleep "${INTERVAL}"
    done
}
agent () {
    . "${HOME}"/bin/agent
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
alias elinks="DISPLAY='' elinks -touch-files -no-connect"
alias hep="site hep"
alias klog="klog -setpag"
alias less="${PAGER}"
alias lfod="site lfod"
alias list="tmux ls"
alias ls="ls -F"
alias mtr="mtr -t"
alias sudo='A=`alias` /usr/bin/sudo '
alias tmux="tmux -S ${TMUX_SOCK}"
alias vi="${VISUAL}"
alias xinit="xinit -- -nolisten tcp"

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
    
# Virtualenv.
OLDPS1="${PS1}"
if [ -r "${HOME}/bin/activate" ]; then
    . "${HOME}/bin/activate"
fi
export PS1="${OLDPS1}"

agent
