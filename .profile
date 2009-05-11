##################  BEGIN HEADERS
# Filename	: $HOME/.profile
# Use		: configures default shell environment
# Author	: Will Maier <willmaier@ml1.net>
# Vim		: :vim: set ft=sh:
# CVS		: $Id: profile,v 1.136 2006/06/26 14:26:44 will Exp $
# Copyright	: Copyright (c) 2005 Will Maier
# License	: BSD; see <http://www.lfod.us/copyright.html>
##################  END HEADERS

# Environment variables
EDITOR=/usr/bin/vi
SHELL_OLD="${SHELL}"
SHELL="$(which zsh 2>/dev/null)" || \
        SHELL="$(which ksh 2>/dev/null)" || \
        SHELL="$(which bash 2>/dev/null)" || \
        SHELL="$(which sh)"

BBCRADIO=mms://livewmstream-ws.bbc.co.uk.edgestreams.net/reflector:43021
CVSEDITOR="${EDITOR}"
CVS_RSH=/usr/bin/ssh
FANOUT=16
HGEDITOR=$HOME/bin/hgeditor
HOSTNAME="$(hostname -s)"
LANG="C"
MAIL_OLD="${MAIL}"
MAIL=""
OCVS=:ext:cvs:/cvs
PAGER=less
PATH="$HOME/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games"
RCMD_CMD="ssh"
RCMD_TEST="yes"
RCMD_TEST_TIMEOUT="3"
RCP_CMD="scp"
RSHPORT="22"
RCMD_USER=${USER}
SSH="$(which ssh)"
if [ "${TERM}" != "screen" ]; then
    TERM=rxvt
fi
TODO=$HOME/TODO/
VERBOSE="1"
TODO="$HOME/TODO"
TMUX_SOCK=~/.tmux.sock
export CLUSTER CVSEDITOR CVS_RSH EDITOR HOSTNAME LANG MAIL PATH
export PAGER RCMD_CMD RCP_CMD SHELL VERBOSE TODO TMUX_SOCK

# Add Python stuff.
PYTHONPATH=~/lib/python2.5/site-packages
[ -d ~/bin ] || mkdir ~/bin
[ -d ~/lib ] || mkdir ~/lib
[ -d "${PYTHONPATH}" ] || mkdir -p "${PYTHONPATH}"
export PYTHONPATH

# Directory containing platform- and host-specific configuration
PROFILES="${HOME}/.profiles"
PLATFORMS="${PROFILES}/platforms"

# Functions
[ -r "${PROFILES}/functions" ] && . "${PROFILES}/functions" || echo "=!=> ${PROFILES}/functions not found!"

# Aliases
[ -r "${PROFILES}/aliases" ] && . "${PROFILES}/aliases" || echo "=!=> ${PROFILES}/aliases not found!"

# Platform-specific settings
if [ "X$(sysctl -n kern.ostype 2>/dev/null)" = "XOpenBSD" -a -r "${PLATFORMS}/openbsd" ]; then
	# Get OpenBSD-specific settings
	. ${PLATFORMS}/openbsd
elif [ "X$(uname -s)" = "XSunOS" -a -r "${PLATFORMS}/solaris" ]; then
	# Get Solaris-specific settings
	. ${PLATFORMS}/solaris
fi

# Host-specific settings

# Initialize environment
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
    alias ssh >/dev/null && unalias ssh
    export CLUSTER RCMD_USER SSH_USER
}

lfod

# Run the preferred shell (unless we're already running it
if [ "${SHELL##*/}" != "${SHELL_OLD##*/}" ]; then
    eval "exec ${SHELL}"
fi
