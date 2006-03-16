##################  BEGIN HEADERS
# Filename	: $HOME/.profile
# Use		: configures default shell environment
# Author	: Will Maier <willmaier@ml1.net>
# Version	: $Revision: 1.121 $
# Updated	: $Date: 2006/03/16 03:38:52 $
# Vim		: :vim: set ft=sh:
# CVS		: $Id: profile,v 1.121 2006/03/16 03:38:52 will Exp $
# Copyright	: Copyright (c) 2005 Will Maier
# License	: Expat; see <http://www.opensource.org/licenses/mit-license.php>
##################  END HEADERS

# profile-ng:
#source $HOME/.profile-ng/environment	    # general environment variables
#source $HOME/.profile-ng/functions	    # general shell functions
#source $HOME/.profile-ng/aliases	    # general aliases
#source $HOME/.profile-ng/arch-specific	    # functions, aliases and variables assigned by
#					    # architecture
#source $HOME/.profile-ng/host-specific	    # functions, aliases and variables assigned by
					    # host

ARCH=`uname`
HOST=$(hostname -s)
VERBOSE=1
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
    fi
export PRINTER

# --[ ENVIRONMENT
    PATH="$HOME/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/X11R6/bin:/opt/:/usr/games/:/usr/pkg/bin/:/usr/pkg/sbin"
    PKG_PATH="ftp://openbsd.mirrors.tds.net/pub/OpenBSD/$(uname -r)/packages/$(uname -m)"
    export PATH
# arch-specific stuff.
case $ARCH in
    SunOS* )
    # fix needed for solaris and BSD
    alias ls='ls -F'
    alias zsh='/afs/engr.wisc.edu/oss/bin/zsh'
    export SHELL='/afs/engr.wisc.edu/oss/bin/zsh'
    ;;
    *BSD )
    alias ls='ls -F'
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
	#alias mail='screen -x mail'
	alias myc='screen -x comms'
	source $HOME/.functions
	export CVSROOT
	;;
	vger* )
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
#    esac
    # PKG_PATH='ftp://openbsd.mirrors.tds.net/pub/OpenBSD/3.8/packages/i386/'

    alias key='source $HOME/.keychain/$HOST-sh'
    alias pource='source $HOME/.profile'
    alias zource='source $HOME/.zshrc'
    alias mtr='mtr -t'

    TODO=$HOME/.brain/todo
    MUSIC=$HOME/.brain/music
    FAQ=$HOME/.brain/faq
    LANG='C'
    MAIL=''
    SHELL=`which zsh`
    CLUSTER=$HOME/.dsh/config
    RCMD_CMD=ssh
    EDITOR=`which vim`
    CVSEDITOR=$EDITOR
export LANG PATH SHELL EDITOR CVSEDITOR MAIL PKG_PATH TODO CLUSTER RCMD_CMD FAQ

# functions
warn () {
    echo "=!=> $1"
}
notify () {
    if [ ${VERBOSE} -ge $1 ]; then
        echo "===> $2"
    fi
}
lookup () {
    grep $1 /usr/share/dict/words
}
net () {
    lsof -Pni
}
mdc () {
    if [ ! -d "$HOME/Maildir" ]; then
	return 0
    fi

    echo "Checking mail at $(date)."

    local FINDCMD="gfind $HOME/Maildir -regex '.*/[a-zA-Z-]+/new/.*' -type f" 
    local SORTCMD="uniq -c"
    for i in $*; do
	case $i in
	    -n)
	    FINDCMD="${FINDCMD} -newer $HOME/Maildir/.marker"
	    ;;
	    -s)
	    SORTCMD="${SORTCMD} | sort -rn"
	esac
    done

    local NUMBER=0
    eval ${FINDCMD} |\
	 sed 's/.*Maildir\///' |\
	 cut -d '/' -f 1 |\
#	 sed 's/.*Maildir\/\([^/]\+\)\/new\/.*/\1/' |\
	 eval ${SORTCMD} |\
	while read i; do
	    echo "      $i"
	    local NEW=$(echo $i | awk '{print $1}')
	    local NUMBER=$((NUMBER + NEW))
	done

    echo "===>  $NUMBER new mails."

    touch $HOME/Maildir/.marker
}
alias mdcs='mdc -s'
alias mnt='mdc -n'
alias mnts='mdc -n -s'
# tdl (todo list) functions
TDL_DATABASE=$HOME/.tdldb
export TDL_DATABASE

alias cae="TDL_DATABASE=$HOME/.tdldb-cae tdl"
alias caea="TDL_DATABASE=$HOME/.tdldb-cae tdla"
alias caed="TDL_DATABASE=$HOME/.tdldb-cae tdld"
alias caeg="TDL_DATABASE=$HOME/.tdldb-cae tdlg"
alias cael="TDL_DATABASE=$HOME/.tdldb-cae tdll"
alias hep="TDL_DATABASE=$HOME/.tdldb-hep tdl"
alias hepa="TDL_DATABASE=$HOME/.tdldb-hep tdla"
alias hepd="TDL_DATABASE=$HOME/.tdldb-hep tdld"
alias hepg="TDL_DATABASE=$HOME/.tdldb-hep tdlg"
alias hepl="TDL_DATABASE=$HOME/.tdldb-hep tdll"
notes () {
    # Determine date/time; used to guess the appropriate class
    DATE=$(date "+%Y.%m.%d") 
    DAY=$(date "+%A") 
    HOUR=$(date "+%H") 
    NOTEPATH=$HOME/School

    case ${DAY} in
	Tuesday|Thursday)
	    case ${HOUR} in
		11|12)
		    CLASS=HS323
		;;
		14|15)
		    CLASS=HS561
		;;
	    esac
	;;
	Monday|Wednesday|Friday)
	    case ${HOUR} in
		08|09)
		    CLASS=HS322
		;;
	    esac
	;;
    esac
    if [ -z ${CLASS} ]; then
	echo "Could not determine class."
	return 1
    fi
    NOTEPATH=${NOTEPATH}/${CLASS}/${DATE}
    vim "+set ft=tex" ${NOTEPATH}
}
alias wiki='ruby $HOME/bin/instiki/instiki.rb --storage $HOME/bin/instiki/storage'
alias portmanager='portmanager --log'
alias elinks="DISPLAY='' elinks -touch-files -no-connect"
#alias vim="DISPLAY='' vim"
alias calendar="calendar -f /home/will/.calendar"
alias ci="ci -l"
alias co="co -l"
CVSROOT=anoncvs@mirror.sg.depaul.edu:/cvs
export CVSROOT
sued () {
    if [ "$(rcsdiff -q -kk $1)" ]; then
	rcsdiff -kk  $1
	read INPUT\?"Commit changes? [Y/n] "
	case ${REPLY} in 
	    y|Y|yes|YES)
	    sudo rcs -l $1
	    sudo ci -u $1
	    ;;
	esac
    fi
    sudo rcs -l $1
    sudo vim $1
    sudo ci -u $1
}
#xpdf () {
#    if [ ! "$1" ]; then 
#	xpdf
#	return 0
#    elif [ "$(echo $1 | egrep "^(http|ftp)")" ]; then
#	FILE=/tmp/xpdf-$(basename $1)
#	ftp -o $FILE $1
#	xpdf $FILE
#	rm $FILE
#    else
#	xpdf $1
#    fi
#}
alias ss="screen -e'^Xx' -S main"
alias sm="screen -x main"
alias out="clear && exit"
alias irc="TZ=UTC irssi"
if [ $(hostname) = "messenger" ]; then
    alias mplayer='/usr/bin/mplayer -nobps -display localhost.localdomain:0 -fs -zoom'
fi
alias commit="cvs ci -m ''"
alias bitl="irc --home=~/.irssi/bitlbee"
pkg_grep () {
    /usr/bin/grep $1 /usr/ports/INDEX-pkg
}
alias exit="clear && exit"
alias sc="TERM=xterm sc"
calc () {
    echo "$*" | bc -l
    #bc -l -e "$*" -e quit
}
alias hours="sc hours.sc"
alias calendar="wyrd 2>/dev/null"
scp-key () {
    # Like: scp-key wcmaier@burrito.cae.wisc.edu "SSH-OPTIONS"
    TARGET=$1
    shift
    SSH_OPTS=$*
    SSH_AUTHFILE='~/.ssh/authorized_keys'
    for PUBKEY in ~/.ssh/id*pub; do
	ssh ${SSH_OPTS} $TARGET "(chmod 600 ${SSH_AUTHFILE}; sh -c \"cat - >> ${SSH_AUTHFILE}\")" < ${PUBKEY}
    done
}
agent () {
    if [ $(find $HOME/.ssh -follow -name "id_[dr]sa" | wc -l | sed -e 's/[^0-9]//g') -lt 1 ]; then
        # There don't appear to be any keys on this host; kill all
        # agents and quit.
        notify 1 "No keys found in $HOME/.ssh."
        pkill -U ${USER} ssh-agent
        return 0
    fi
    SSH_AGENT_FILE=~/.ssh/agent
    if [ -f ${SSH_AGENT_FILE} ]; then
        notify 2 "Found agent file ${SSH_AGENT_FILE}."
        chmod 600 ${SSH_AGENT_FILE}
        . ${SSH_AGENT_FILE} > /dev/null

        # See if old agent still exists.
        kill -0 ${SSH_AGENT_PID} > /dev/null 2>&1
        if [ $? -gt 0 ]; then
            notify 2 "Agent ${SSH_AGENT_PID} doesn't appear to be running."
            # Agent doesn't exist; make new agent.
            rm -f ${SSH_AGENT_FILE}
            eval $(ssh-agent -s | tee ${SSH_AGENT_FILE})
        else
            notify 2 "Agent ${SSH_AGENT_PID} exists."
            # Agent does exist; kill all the other 'uns.
            notify 2 "kill $(pgrep -U ${USER} ssh-agent | grep -v ${SSH_AGENT_PID})"
            kill $(pgrep -U ${USER} ssh-agent | grep -v ${SSH_AGENT_PID}) > /dev/null 2>&1
        fi
    else
        notify 2 "Didn't find an agent file."
        # If we don't know about the agents floating around, kill
        # 'em.
        notify 2 "pkill -U ${USER} ssh-agent"
        pkill -U ${USER} ssh-agent

        # Make new agent
        notify 2 "eval $(ssh-agent -s | tee ${AGENTFILE})"
        eval $(ssh-agent -s | tee ${AGENTFILE})
    fi

    # List keys represented by the agent; if none, add some keys
    if [ ${VERBOSE} -ge 1 ]; then
        ssh-add -l
    else
        ssh-add -l 2>&1 > /dev/null
    fi
    if [ $? -gt 0 ]; then
        notify 2 "Don't appear to be any keys in the agent."
        notify 2 "ssh-add -t 4h > /dev/null 2>&1"
        ssh-add -t 4h > /dev/null 2>&1
    fi
    chmod 600 ${SSH_AGENT_FILE}
}
pkg_find () {
    grep -iE $1 /usr/ports/index.txt
}
pkgin () {
    sudo pkg_add $PKG_PATH/$1
}
CLUSTER=~/.cluster
RCMD_CMD=ssh
alias lsof="sudo lsof"
if [ "$UID" -gt "0" ]; then
    VERBOSE=0 agent
fi
ssh-rmkey () {
    LINE=$1
    TMPFILE=$(mktemp -q ~/.ssh/known_hosts.XXXX || exit 1)
    sed -e "${LINE}d" ~/.ssh/known_hosts >| ${TMPFILE}
    mv ${TMPFILE} ~/.ssh/known_hosts
}
todo () {
    vim $TODO
}
faq () {
    vim $FAQ
}
# find /dev -user $USER -wholename "*/tty*" -o -wholename "*/pts/*" 2>/dev/null | while read FILE; do stat -c "%Z" $FILE; done
