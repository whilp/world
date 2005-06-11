# ~/.profile
# Will Maier <willmaier@ml1.net>
# 2005.06.11

# --[ ENVIRONMENT
    # Standard vars
    LANG="C"
    SHELL=/usr/bin/zsh
    EDITOR=/usr/bin/vim
    CVSEDITOR=$EDITOR
    PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/X11R6/bin:/opt/:/usr/games/:/home/will/bin
    # Set special vars for CAE machines
    case $HOST in
	nacho,bender)
	stty erase ^?
	PATH=/opt/SUNWspro/bin:/usr/ccs/bin:/usr/bin:/bin:/usr/local/bin:/opt/sfw/bin:/usr/sfw/bin:/usr/afsws/bin:/usr/openwin/bin:/usr/X11R6/bin:/afs/engr.wisc.edu/oss/bin:/afs/engr.wisc.edu/apps/bin:/afs/engr.wisc.edu/common/scripts:/afs/engr.wisc.edu/local/bin:/usr/ucb:/usr/dt/bin
	OSS=/afs/engr.wisc.edu/oss
	LD_LIBRARY_PATH=/opt/SUNWspro/bin:/usr/ccs/bin:/usr/bin:/bin:/usr/local/bin:/opt/sfw/bin:/usr/sfw/bin:/usr/afsws/bin:/usr/openwin/bin:/usr/X11R6/bin:/afs/engr.wisc.edu/oss/bin:/afs/engr.wisc.edu/apps/bin:/afs/engr.wisc.edu/common/scripts:/afs/engr.wisc.edu/local/bin:/usr/ucb:/usr/dt/bin
	MANPATH=/usr/share/man:usr/local/man:/opt/sfw/man:/usr/openwin/man/usr/X11R6/man:/afs/engr.wisc.edu/oss/man:/afs/engr.wisc.edu/apps/man
	PYTHONPATH=$OSS/lib/python:$OSS/lib/python/lib-dynload:$OSS/lib/python/site-packages
	export OSS PYTHONPATH LD_LIBRARY_PATH MANPATH
	;;
    esac
export LANG PATH SHELL EDITOR CVSEDITOR

# --[ ALIASES
if [ "$TERM" != "dumb" ]; then
    eval `dircolors -b`
    alias ls='ls --color=auto'
fi
alias xlockb='xlock -mode blank'
alias s='screen'

# --[ CVS
    # Personal CVS
    CVSROOT=":ext:will@phnx.ath.cx:/cvs"
    # CAE CVS
    # CVSROOT=/afs/engr.wisc.edu/common/repository
export CVSROOT

# --[ PRINTING
    if [ -x /afs/engr.wisc.edu/common/scripts/default_printer ] ; then
	PRINTER=`/afs/engr.wisc.edu/common/scripts/default_printer`
    else
	PRINTER=ps
    fi
export PRINTER
