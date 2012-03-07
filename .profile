WHICH=$(/usr/bin/env which which)
# Environment variables
EDITOR="$($WHICH vim 2>/dev/null)" || \
    EDITOR="$($WHICH nvi 2>/dev/null)" || \
    EDITOR="$($WHICH vi)"
VISUAL=${EDITOR}
SHELL_OLD="${SHELL}"
SHELL="$($WHICH bash 2>/dev/null)" || \
    SHELL="$($WHICH ksh 2>/dev/null)" || \
    SHELL="$($WHICH sh)"
HISTFILE=~/.history
HOSTNAME="$(hostname -s)"
LANG="en_US.UTF-8"
LESSHISTFILE=
OLDMAIL="${MAIL}"
MAIL=""
PAGER="less -iX"
PATH="$HOME/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games"
SSH="$(which ssh)"
TMUX_SOCK=~/.tmux/sock

export EDITOR ENV HISTFILE HOSTNAME LANG LESSHISTFILE 
export MAIL OLDMAIL PATH PAGER SHELL TMUX_SOCK
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
        . ~/.profile
    fi
}
screenshot () {
    xwd -root | xwdtopnm | pnmtopng "$1"
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
alias tmux="tmux -S ${TMUX_SOCK}"
alias vi="${VISUAL}"
alias vimdiff="vimdiff -o"
alias xinit="xinit -- -nolisten tcp"
alias gist="(cd ~/share/gist && ./gist)"

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

# Apply site-specific settings.
GIT_COMMITTER_NAME="Will Maier"
case "${SITE}" in
    hep)    HGUSER="Will Maier <wcmaier@hep.wisc.edu>"
            GIT_COMMITTER_EMAIL="wcmaier@hep.wisc.edu"
            ;;
    *)      HGUSER="Will Maier <wcmaier@m.aier.us>"
            GIT_COMMITTER_EMAIL="wcmaier@m.aier.us"
            ;;
esac
GIT_AUTHOR_NAME=$GIT_COMMITTER_NAME
GIT_AUTHOR_EMAIL=$GIT_COMMITTER_EMAIL
export HGUSER GIT_AUTHOR_NAME GIT_AUTHOR_EMAIL GIT_COMMITTER_NAME GIT_COMMITTER_EMAIL

# SSH directories.
(cd ~/; mkdir -p .ssh .ssh/controls .ssh/callbacks) 2>/dev/null

# Set SSH_CLIENT_NAME after host has set up callback, if necessary.
SSH_CLIENT_NAME="${HOSTNAME}"
export SSH_CLIENT_NAME
    
# Virtualenv.
OLDPS1="${PS1}"
if [ -r "${HOME}/bin/activate" ]; then
    . "${HOME}/bin/activate"
fi
export PS1="${OLDPS1}"

# Local::lib
if [ -d "${HOME}/perl5/lib/perl5" ]; then
    eval $(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)
fi

agent
ulimit -n 1023

BOLD=$(tput bold)
UNBOLD=$(tput sgr0)

ps1 () {
    if [ -n "$*" ]; then
        ARG=$1; OPTARG=$2
        case "${ARG}" in
            -s) export PS1SESSION="${OPTARG}";;
            -l) case "${OPTARG}" in
                    [+-]*) MAXPS1LEN="$((${MAXPS1LEN:-0} + ${OPTARG}))";;
                    *) MAXPS1LEN="${OPTARG}";;
                esac;;
        esac
        return 0
    fi

    typeset PS1PWD=
    typeset PS1SESSION="${PS1SESSION:-${HOSTNAME}}"
    typeset MAXPS1LEN=$((${MAXPS1LEN:-20} - 5 - ${#PS1SESSION}))
    case "${PWD}" in
        "${HOME}") PS1PWD='~';;
        "${HOME}"/*) PS1PWD='~'"${PWD#${HOME}}";;
        *) PS1PWD="${PWD}";;
    esac

    typeset TILDE=
    if [ -z "${PS1PWD%%\~*}" ]; then
        TILDE="~"
        PS1PWD="${PS1PWD#\~}"
        MAXPS1LEN=$((${MAXPS1LEN} - 1))
    fi
    typeset LAST="${PS1PWD##*/}"
    typeset TRUNCATE=
    if [ "${#LAST}" -gt "${MAXPS1LEN}" ]; then
        # Truncate.
        typeset -R"$((${MAXPS1LEN} + 1))" TMPLAST="${LAST}"
        TRUNCATE=1
        LAST="${TMPLAST#?}"
    else
        while [ -n "${PS1PWD}" -a "${#LAST}" -lt "${MAXPS1LEN}" ]; do
            PS1PWD="${PS1PWD%/*}"
            LAST="${PS1PWD##*/}/${LAST#/}"
        done
        typeset -R"${MAXPS1LEN}" TMPLAST="${LAST}"
        if [ -n "${PS1PWD}" ]; then
            LAST="${TMPLAST}"
            TRUNCATE=1
        fi
    fi
    if [ -n "${TRUNCATE}" ]; then
        TRUNCATE="${UNBOLD}<${BOLD}"
        TILDE=
    fi
    PS1PWD="${TRUNCATE}${TILDE}${LAST%/}"
    typeset DOLLAR="$"
    case "${USER}" in root) DOLLAR="#";; esac

    echo -ne "\a${PS1SESSION}:${BOLD}${PS1PWD}${UNBOLD} ${DOLLAR}"
}

PS1="\$(ps1) "
export PS1

# Run the preferred shell (unless we're already running it).
if [ "${SHELL##*/}" != "${SHELL_OLD##*/}" ]; then
    eval "exec ${SHELL} -l"
fi
