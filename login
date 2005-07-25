# Filename	: $HOME/.login
# Use		: configures environment on shell login
# Author	: Will Maier <willmaier@ml1.net>
# Updated	: 2005.07.25 11:44:45

if ( ! $?TERMCAP ) then
	tset -Q  '-mdialup:?vt100' $TERM
endif

stty	newcrt crterase

set	savehist=100
set	ignoreeof

setenv	EXINIT		'set ai sm noeb'
setenv	HOSTALIASES	 $HOME/.hostaliases
setenv  PKG_PATH	'ftp://openbsd.mirrors.tds.net/pub/OpenBSD/3.7/packages/i386/'

if ( -x /usr/games/fortune) /usr/games/fortune
