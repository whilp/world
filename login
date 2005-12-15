##################  BEGIN HEADERS
# Filename	: $HOME/.login
# Use		: configures environment on shell login
# Version	: $Revision: 1.6 $
# Author	: Will Maier <willmaier@ml1.net>
# Updated	: $Date: 2005/12/15 17:26:32 $
# CVS		: $Id: login,v 1.6 2005/12/15 17:26:32 will Exp $
# Copyright	: Copyright (c) 2005 Will Maier
# License	: Expat; see <http://www.opensource.org/licenses/mit-license.php>
##################  END HEADERS

if ( ! $?TERMCAP ) then
	tset -Q  '-mdialup:?vt100' $TERM
endif

# Console keyboard config
case $ARCH in
    OpenBSD)
    wsconsctl keyboard.map+="keysym Caps_Lock = Control_L"
    ;;
esac

stty	newcrt crterase
stty	erase 

set	savehist=100
set	ignoreeof

setenv	EXINIT		'set ai sm noeb'
setenv	HOSTALIASES	 $HOME/.hostaliases
setenv  PKG_PATH	'ftp://openbsd.mirrors.tds.net/pub/OpenBSD/3.7/packages/i386/'

if ( -x /usr/games/fortune) /usr/games/fortune
