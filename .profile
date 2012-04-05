WHICH=$(/usr/bin/env which which)
# Environment variables
EDITOR="$($WHICH vim 2>/dev/null)" || \
    EDITOR="$($WHICH nvi 2>/dev/null)" || \
    EDITOR="$($WHICH vi)"
VISUAL=${EDITOR}

HISTFILE=~/.history
HOSTNAME="$(hostname -s)"
LANG="en_US.UTF-8"
LESSHISTFILE=
OLDMAIL="${MAIL}"
MAIL=""
PAGER="less -iX"
SSH="$(which ssh)"
TMUX_SOCK=~/.tmux/sock

export EDITOR HISTFILE HOSTNAME LANG LESSHISTFILE 
export MAIL OLDMAIL PAGER TMUX_SOCK
unset WHICH

# CVS.
CVSEDITOR="${EDITOR}"
CVS_RSH=/usr/bin/ssh

export CVSEDITOR CVS_RSH

# Functions.
addto () {
    local STRING=$1
    local NEW=$2
    local AFTER=$3
    case "${STRING}" in 
        ${NEW}|${NEW}:*|*:${NEW}|*:${NEW}:*);;
        *) [ "${AFTER}" = "after" ] && STRING="${STRING}:${NEW}" || STRING="${NEW}:${STRING}" ;; 
    esac
    echo ${STRING}
}

addtopath () {
    local item="$1"
    [ -d "$item" -a -x "$item" ] || return
    export PATH=$(addto "$item" "$PATH")
}

sleepuntil () {
    local DATE=$1 
    local INTERVAL=${2:-60} 
    local TARGET=$(date -j "${DATE}" "+%s" 2>/dev/null) 
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
alias less="${PAGER}"
alias lfod="site lfod"
alias list="tmux ls"
alias ls="ls -F"
alias simple='site simple'
alias sudo='A=`alias` /usr/bin/sudo '
alias tmux="tmux -S ${TMUX_SOCK}"
alias vi="${VISUAL}"
alias vimdiff="vimdiff -o"
alias gist="(cd ~/share/gist && ./gist)"

path="
$HOME/bin
$HOME/Library/Haskell/bin
/usr/texbin
/bin
/sbin
/usr/bin
/usr/sbin
/usr/X11R6/bin
/usr/local/bin
/usr/local/sbin
/usr/games
"
for d in $path; do addtopath $d; done

libs="
$HOME/lib
"
for d in $libs; do [ -d "$d" ] && export LD_LIBRARY_PATH=$(addto "$LD_LIBRARY_PATH" "$d"); done

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
    simple) HGUSER="Will Maier <will@simple.com>"
            GIT_COMMITTER_EMAIL="will@simple.com"
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
OLDPS1="$PS1"
if [ -r "${HOME}/bin/activate" ]; then
    . "${HOME}/bin/activate"
fi
if [ -n "$OLDPS1" ]; then
    export PS1="$OLDPS1"
else
    unset PS1
fi

# Local::lib
if [ -d "${HOME}/perl5/lib/perl5" ]; then
    eval $(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)
fi

agent
ulimit -n 1023

bold="\[\033[1m\]"
off="\[\033[m\]"
export PS1="\a\h:$bold\w$off \$ "

case "$SHELL" in
	*ksh*) export ENV=~/.kshrc;;
esac
