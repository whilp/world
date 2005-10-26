##################  BEGIN HEADERS
# Filename	: $HOME/.profile
# Use		: configures default shell environment
# Author	: Will Maier <willmaier@ml1.net>
# Updated	: 2005.10.05 07:50:58 -0500
##################  END HEADERS

ARCH=`uname`
#ISSUE=`awk '{print $1}' /etc/issue || echo "unknown"`

# --[ CVS
    # Personal CVS
    CVSROOT=":ext:will@merk:/cvs"
    # CAE CVS
    # CVSROOT=/afs/engr.wisc.edu/common/repository
    CVS_RSH=ssh
export CVSROOT CVS_RSH
alias mycvs='export CVSROOT=":ext:will@phnx.ath.cx:/cvs"'
alias caecvs='export CVSROOT=/afs/engr.wisc.edu/common/repository'

# --[ PRINTING
    if [ -x /afs/engr.wisc.edu/common/scripts/default_printer ] ; then
	PRINTER=`/afs/engr.wisc.edu/common/scripts/default_printer`
    else
	PRINTER=ps
    fi
export PRINTER

# --[ ENVIRONMENT
    PATH="$HOME/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/X11R6/bin:/opt/:/usr/games/"
    export PATH
# arch-specific stuff.
case $ARCH in
    SunOS* )
    # fix needed for solaris and BSD
    alias ls='gls --color=auto -F'
    alias zsh='/afs/engr.wisc.edu/oss/bin/zsh'
    alias tar='gtar'
    export SHELL='/afs/engr.wisc.edu/oss/bin/zsh'
    ;;
    *BSD )
    alias ls='gnuls --color=auto -F'
    alias tar='gtar'
    ;;
    * )
    alias ls='ls --color=auto -F'
    ;;
esac

# host-specific
    case $HOST in
	nacho* | bender* | anon* | mace* )
	# Set special vars for CAE machines
	TERM=ansi
	PATH="/opt/SUNWspro/bin:/usr/ccs/bin:/usr/bin:/usr/sbin:/sbin:/bin:/usr/local/bin:/usr/local/sbin:/opt/sfw/bin:/usr/sfw/bin:/usr/afsws/bin:/usr/afsws/sbin:/usr/openwin/bin:/usr/X11R6/bin:/afs/engr.wisc.edu/oss/bin:/afs/engr.wisc.edu/apps/bin:/afs/engr.wisc.edu/common/scripts:/afs/engr.wisc.edu/local/bin:/usr/ucb:/usr/dt/bin:$HOME/bin"
	OSS='/afs/engr.wisc.edu/oss'
	LD_LIBRARY_PATH='/opt/SUNWspro/bin:/usr/ccs/bin:/usr/bin:/bin:/usr/local/bin:/opt/sfw/bin:/usr/sfw/bin:/usr/afsws/bin:/usr/openwin/bin:/usr/X11R6/bin:/afs/engr.wisc.edu/oss/bin:/afs/engr.wisc.edu/apps/bin:/afs/engr.wisc.edu/common/scripts:/afs/engr.wisc.edu/local/bin:/usr/ucb:/usr/dt/bin'
	MANPATH='/usr/share/man:usr/local/man:/opt/sfw/man:/usr/openwin/man/usr/X11R6/man:/afs/engr.wisc.edu/oss/man:/afs/engr.wisc.edu/apps/man'
	PYTHONPATH="$OSS/lib/python:$OSS/lib/python/lib-dynload:$OSS/lib/python/site-packages"
	SENDMAIL='sendmail'
	alias python="/afs/engr.wisc.edu/oss/bin/python"
	alias dot='`/afs/engr.wisc.edu/common/scripts/dot`'
	alias klog='klog -setpag'
	export OSS PYTHONPATH LD_LIBRARY_PATH MANPATH TERM SENDMAIL
	;;
	merkur* )
	CVSROOT=/cvs
	alias agent='keychain --timeout 120 ~/.ssh/id_rsa; key'
	alias mail='screen -x mail'
	alias myc='screen -x comms'
	source $HOME/.functions
	export CVSROOT
	;;
	vger* )
	alias agent='keychain ~/.ssh/id_rsa; key'
	;;
	haya* )
	alias ls='ls -F'
	;;
    esac

# os-specific
#    case $ISSUE in
#	*Ubuntu* )
#	alias upgrade='sudo apt-get update; sudo apt-get dist-upgrade'
#	;;
#	*BSD* )
#	PKG_PATH='ftp://openbsd.mirrors.tds.net/pub/OpenBSD/3.7/packages/i386/'
#    esac

    alias key='source $HOME/.keychain/$HOST-sh'
    alias agent='keychain ~/.ssh/id_rsa'
    alias pource='source $HOME/.profile'
    alias zource='source $HOME/.zshrc'
    alias todo='grep -v DONE $HOME/TODO'
    alias etodo='vim $HOME/TODO'
    alias mtr='mtr -t'
    alias xterm='rxvt'
    alias grep='grep -IHn'
    alias mdcs='mdc | sort -rn'
    alias mnts='mnt | sort -rn'

    TODO=$HOME/TODO
    LANG='C'
    MAIL=''
    SHELL=`which zsh`
    CLUSTER=$HOME/.dsh/config
    RCMD_CMD=ssh
    EDITOR=`which vim`
    CVSEDITOR=$EDITOR
export LANG PATH SHELL EDITOR CVSEDITOR MAIL PKG_PATH TODO CLUSTER RCMD_CMD

# functions
lookup () {
    grep $1 /usr/share/dict/words
}
net () {
    lsof -Pni
}
mdc () {
    if [ "${HOST}" != "merkur" ]; then
	ssh merk "source .profile && $0 $1"
	return 0
    fi
    if [ ! -d "$HOME/Maildir" ]; then
	return 0
    fi

    local FINDCMD="find $HOME/Maildir -regex '.*/[a-zA-Z-]+/new/.*' -type f" 
    for i in $*; do
	case $i in
	    -n)
	    local FINDCMD="${FINDCMD} -newer $HOME/Maildir/marker"
	    ;;
	esac
    done

    eval ${FINDCMD} |\
	 sed 's/.*Maildir\/\([^/]\+\)\/new\/.*/\1/' |\
	 sort |\
	 uniq -c
    touch $HOME/Maildir/.marker
}
mnt () {
    mdc -n
}
