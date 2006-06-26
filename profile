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
EDITOR="$(which vim 2>/dev/null)" || \
	EDITOR="$(which vi)"
SHELL_OLD="${SHELL}"
SHELL="$(which zsh 2>/dev/null)" || \
        SHELL="$(which ksh 2>/dev/null)" || \
        SHELL="$(which bash 2>/dev/null)" || \
        SHELL="$(which sh)"

CLUSTER="${HOME}/.dsh/config"
CVSEDITOR="${EDITOR}"
CVS_RSH="$(which ssh)"
HOSTNAME="$(hostname -s)"
LANG="C"
MAIL_OLD="${MAIL}"
MAIL=""
PATH="$HOME/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games"
RCMD_CMD="${CVS_RSH}"
RCP_CMD="$(which scp)"
TODO=$HOME/TODO/
VERBOSE="1"
TODO="$HOME/TODO"
export CLUSTER CVSEDITOR CVS_RSH EDITOR HOSTNAME LANG MAIL PATH RCMD_CMD RCP_CMD SHELL VERBOSE TODO

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

# Run the preferred shell (unless we're already running it
if [ "${SHELL##*/}" != "${SHELL_OLD##*/}" ]; then
    eval "exec ${SHELL}"
fi
