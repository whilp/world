WHICH=$(/usr/bin/env which which)
# Environment variables
EDITOR="$($WHICH nvi 2>/dev/null)" || \
    EDITOR="$($WHICH vi)" || \
    EDITOR="$($WHICH vim)"
VISUAL=${EDITOR}
SHELL_OLD="${SHELL}"
SHELL="$($WHICH zsh 2>/dev/null)" || \
        SHELL="$($WHICH ksh 2>/dev/null)" || \
        SHELL="$($WHICH bash 2>/dev/null)" || \
        SHELL="$($WHICH sh)"
HOSTNAME="$(hostname -s)"
LANG="C"
OLDMAIL="${MAIL}"
MAIL=""
PAGER="less -iX"
PATH="$HOME/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games"
SSH="$(which ssh)"
TMUX_SOCK=~/.tmux.sock
UID=${UID:-$(id -u)}
VERBOSE="1"

export EDITOR HOSTNAME LANG MAIL PATH PAGER SHELL VERBOSE TODO TMUX_SOCK

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
[ -r "${PROFILES}/functions" ] && . "${PROFILES}/functions"

# Aliases.
[ -r "${PROFILES}/aliases" ] && . "${PROFILES}/aliases"

# Platform settings.
UNAME=$(uname)
case "${UNAME}" in
    OpenBSD)    PLATFORM="${PLATFORMS}/openbsd";;
    Linux)      PLATFORM="${PLATFORMS}/linux";;
esac
[ -r "${PLATFORM}" ] && . "${PLATFORM}"

# Host settings.
FQDN=$(hostname)
[ -r "${HOSTS}/${FQDN}" ] && . "${HOSTS}/${FQDN}"
    
# Virtualenv.
OLDPS1="${PS1}"
if [ -r "${HOME}/bin/activate" ]; then
    . "${HOME}/bin/activate"
fi
export PS1="${OLDPS1}"

# Run the preferred shell (unless we're already running it).
if [ "${SHELL##*/}" != "${SHELL_OLD##*/}" ]; then
    eval "exec ${SHELL}"
fi
