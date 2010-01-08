# Environment variables
EDITOR="$(which nvi 2>/dev/null)" || \
    EDITOR="$(which vi)" || \
    EDITOR="$(which vim)"
VISUAL=${EDITOR}
SHELL_OLD="${SHELL}"
SHELL="$(which zsh 2>/dev/null)" || \
        SHELL="$(which ksh 2>/dev/null)" || \
        SHELL="$(which bash 2>/dev/null)" || \
        SHELL="$(which sh)"
HOSTNAME="$(hostname -s)"
LANG="C"
OLDMAIL="${MAIL}"
MAIL=""
PAGER="less -iX"
PATH="$HOME/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games"
SSH="$(which ssh)"
if [ "${TERM}" != "screen" ]; then
    TERM=vt220
fi
VERBOSE="1"
TMUX_SOCK=~/.tmux.sock
export EDITOR HOSTNAME LANG MAIL PATH PAGER SHELL VERBOSE TODO TMUX_SOCK

# CVS.
CVSEDITOR="${EDITOR}"
CVS_RSH=/usr/bin/ssh
OCVS=:ext:cvs:/cvs

export CVSEDITOR CVS_RSH OCVS

# dsh.
FANOUT=16
RCMD_CMD="ssh"
RCMD_TEST="yes"
RCMD_TEST_TIMEOUT="3"
RCMD_USER=${USER}
RCP_CMD="scp"
RSHPORT="22"

export FANOUT RCMD_CMD RCMD_TEST RCMD_TEST_TIMEOUT RCMD_USER RCP_CMD RSHPORT

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
[[ -r "${PLATFORM}" ]] && . "${PLATFORM}"

# Host settings.
FQDN=$(hostname)
[[ -r "${HOSTS}/${FQDN}" ]] && . "${HOSTS}/${FQDN}"
    
# Virtualenv.
OLDPS1="${PS1}"
if [ -r "${HOME}/bin/activate" ]; then
    . "${HOME}/bin/activate"
fi
export PS1="${OLDPS1}"

# Initialize ssh-agent.
if [ "${UID}" -gt "0" ]; then
    VERBOSE=0 agent
fi

# Set up environment.
hep () {
    CLUSTER=~/.dsh/config-hep
    RCMD_USER="wcmaier"
    export CLUSTER RCMD_USER SSH_USER
    alias ssh="ssh -l ${RCMD_USER}"
}
lfod () {
    CLUSTER=~/.dsh/config
    RCMD_USER="will"
    alias | grep -q '^ssh' && unalias ssh
    export CLUSTER RCMD_USER SSH_USER
}

lfod

# Run the preferred shell (unless we're already running it).
if [ "${SHELL##*/}" != "${SHELL_OLD##*/}" ]; then
    eval "exec ${SHELL}"
fi
