#! /bin/sh

# $Id: obsd-upgrade.sh,v 1.1 2005/12/17 15:50:08 will Exp $

# Copyright 2003 - 2005 by Han Boetes <han@mijncomputer.nl>
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the "Software"), to deal in the Software without
# restriction, including without limitation the rights to use,
# copy, modify, merge, publish, distribute, sublicense, and/or
# sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following
# conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
# OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.

# TODO
# Pathnames may change during the update. Look for a way around that.
#

config_found=false
if [ -r /etc/OpenBSD-binary-upgrade.rc ]; then
    . /etc/OpenBSD-binary-upgrade.rc
    config_found=true
fi
if [ -r ~/.OpenBSD-binary-upgrade.rc ]; then
    . ~/.OpenBSD-binary-upgrade.rc
    config_found=true
fi
if [ $config_found = false ]; then
    echo 'No config found, check your instalation!' >&2
    exit 1
fi

PATH='/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin'
name=${0##*/}
restart=false

# Check if the alias beep is defined.
if ! alias beep > /dev/null; then
    alias beep=:
fi

# Check if the function afterscript is defined
if ! typeset -f afterscript > /dev/null; then
    alias afterscript=:
fi

wait_for_user()
{
    echo -n 'Press enter to continue.'
    read nop
}

message()
{
    echo "$*"
}

warning()
{
    echo "==warning==> $*" >&2
}

error()
{
    echo "===error===> $*" >&2
}


re_boot()
{
    echo -n "Reboot? [y/N] "
    read answer
    case "$answer" in
	y*|Y*) $SUDO /sbin/reboot ;;
	*) : ;;
    esac
}

check_version()
{
    # Don't forget to add [] to fool awk.
    latest_version=$(lynx -dump http://www.xs4all.nl/\~hanb/software/OpenBSD-binary-upgrade/OpenBSD-binary-upgrade | awk '/Open[B]SD-binary-upgrade,v/ {print $4}')
    this_version=$(awk '/Open[B]SD-binary-upgrade,v/ {print $4}' $0)
    latest_version=${latest_version#1.}
    this_version=${this_version#1.}
    if [ $latest_version -gt $this_version ]; then
	cat << EOF

A newer version of $name has been found.  Go to

   http://www.xs4all.nl/\~hanb/software/OpenBSD-binary-upgrade/

to get the latest version.

EOF
    else
	echo "$name is up to date"
    fi
    exit 0
}

case "$1" in
    -f)
        # get the filelist
        eval $LIST_COMMAND
	echo -n "It's now......................... "
	date '+%h %d %H:%M'
	echo -n "And your last install was from... "
	ls -l /bsd|cut -d' ' -f9-12
        exit 0
        ;;
    -r) restart=true ;;
    -c)
	check_version ;;
    '')
	: # no argument, default.
        ;;
    *)
        cat << EOF
Usage: $name [-f] [-r] [-c] [-h]
Update to the latest snapshot/release in a comfortable way.

  -f   Show the filelist from the mirror.
  -r   Use this option after you rebooted.
  -c   Check if you are using the latest version.
  -h   This help.

EOF
        exit 0
	;;
esac

for i in $BASE $X11; do
    PACKAGES="$PACKAGES $i$BSDVERSION.tgz"
done

FETCHPACKAGES="$KERNEL $PACKAGES"

if [ ! -d "$DOWNLOADDIR" ]; then
    mkdir -p $DOWNLOADDIR || exit 1
fi
if ! cd $DOWNLOADDIR; then
    error "$DOWNLOADDIR is not accessible."
    exit 1
fi

if find $PWD -perm -0002 -prune|grep -q $PWD; then
    error "$PWD is a world-writable directory, that's too insecure."
    exit 1
fi

if ! type -p ${FETCH%% *} > /dev/null 2>&1; then
    error "You don't have the fetchprogram ${FETCH%% *} installed."
    error "Please install that port/package or check your configuration."
    exit 1
fi

if [ "$restart" = false ]; then
    # Keep etc??.tgz file for mergeslave and nag user about it if he
    # doesn't have it.
    # XXX Two etc??.tgz
    if [ -e etc??.tgz ]; then
	mv -i etc??.tgz etc_old$BSDVERSION.tgz
    else
	warning "Don't delete etc$BSDVERSION.tgz after you're done with me."
	warning "If you can still get your hands on the etc\?\?.tgz that"
	warning "goes with your current please copy it to $DOWNLOADDIR"
	wait_for_user
	if [ -e etc??.tgz ]; then
	    mv -i etc??.tgz etc_old$BSDVERSION.tgz
	elif ! type -p mergemaster > /dev/null 2>&1; then
	    error "This script requires mergemaster."
	    error "Please install that port/package."
	    exit 1
	fi
    fi

    # Only when the downloader cannot do timestamping it's necesarry to
    # check if people want to remove old packages.
    case "$FETCH" in
	*wget*|*rsync*)
            for i in $FETCHPACKAGES; do
		FETCHSTRING="$FETCHSTRING $MIRROR/$i"
            done
            FETCHSTRING="$FETCHSTRING $MIRROR/MD5"
            ;;
	*)
            # Make sure we have the latest MD5 sums
            rm -f MD5
            FETCHPACKAGES="$FETCHPACKAGES MD5"
            unset all
            for i in $FETCHPACKAGES; do
		if [ -e "$i" ]; then
                    if [ -z $all ];then
			echo -n "$i already exists;" \
			" Delete/delete aLl/Use/use All [D/L/U/A] "
			read answer
		    fi
                    case "$answer" in
			d*|D*)
			    echo Removing $i
			    rm -f $i
			    ;;
			a*|A*) break ;;
			l*|L*)
			    echo Removing $i
			    all=yes
			    rm -f $i
			    ;;
			*) : ;;
                    esac
		fi
            done
            for i in $FETCHPACKAGES; do
		if [ ! -e "$i" ]; then
                    FETCHSTRING="$FETCHSTRING $MIRROR/$i"
		fi
            done
            ;;
    esac


    # Alas wget still logs in for every single file for ftp-servers. Blame
    # ftp.
    echo "Fetchin' $FETCHSTRING"
    case "$FETCH" in
	*rsync*)
            for file in $FETCHSTRING; do
		$FETCH $file .
		if [ $? -ne 0 ]; then
                    error 'Download failed, aborting.' >&2
                    exit 1
		fi
            done
            ;;
	*)
            $FETCH $FETCHSTRING
            if [ $? -ne 0 ]; then
		error 'Download failed, aborting.' >&2
		exit 1
            fi
            ;;
    esac


    if [ "$CheckMD5" = yes ]; then
	echo 'Testing if all MD5-sums are valid.'
	unset abort
	for i in ${FETCHPACKAGES%MD5}; do
	    set $(md5 $i)
	    md5is=$4
	    set $(grep \($i\) MD5)
	    md5shouldbe=$4
            if [ "$md5is" != "$md5shouldbe" ]; then
		if [ -z "$md5shouldbe" ]; then
                    echo "Warning, I don't have any md5 info for $i"
		else
                    echo "The md5sum for $i is $md5is and" \
			" it should be $md5shouldbe" >&2
                    abort=yes
		fi
            else
		echo "The md5sum for $i is OK."
            fi
	done
	if [ "$abort" = yes ]; then
            echo 'Error, aborting' >&2
            exit 1
	fi
    fi

    #
    # First install stage.
    #
    beep

    for i in $KERNEL; do
	if [ -e "/$i" ]; then
	    $SUDO mv -f /$i /o$i || exit 1
	fi
	echo "Installing $i in /"
	$SUDO install -p $KERNEL / || exit 1
    done

    cat << EOF

If you are upgrading from a previous release, or a flagday has
passed since the previous release/snapshot you should reboot
now.  You may skip this if you are sure this is not the case.

After the reboot you MUST start $name again with the -r option.

EOF
    re_boot

fi # ! restart

#
# Second install stage.
#

for i in $PACKAGES; do
    if [ $i != ${i#comp} ]; then
        # Remove all files in include since they have no installdate
        # based timestamp. And they will be restored right after this.
        $SUDO rm -rf /usr/include
    fi
    if [ $i = ${i#etc} -a $i = ${i#base} -a $i = ${i#xetc} ]; then
        message "Extracting $i in /"
	$SUDO tar xzfhp $i -C / || exit 1
    fi
done

# Finaly install base.
if [ "${PACKAGES#*base*}" != "$PACKAGES" ]; then
    message "Extracting base$BSDVERSION.tgz in /"
    $SUDO tar xzfhp base$BSDVERSION.tgz -C / || exit 1
fi

#
# mergeslave/mergemaster; two ways to merge files in /etc
#
if [ -r etc??.tgz -a -r etc_old$BSDVERSION.tgz ]; then

    # No need for users to configure this.
    DIFFFILE='etc.diff'

    check4rej()
    {
	if grep "${1#/}.rej" $REJECTS > /dev/null; then
	    warning "I found a reject for $1"
	    warning "Please fix this problem right now, so I can proceed."
	    wait_for_user
	fi
    }

    message "Running mergeslave to update /etc etc."

    rm -rf new/ old/
    mkdir new/ old/
    tar xzf etc$BSDVERSION.tgz -C new/
    tar xzf etc_old$BSDVERSION.tgz -C old/

    # Remove own files.
    tmproots='new/ old/'
    # These files will not be examined for changes nor updated.
    for tmproot in $tmproots; do
	for i in $files; do
	    rm -f $tmproot$i
	done
	for i in $dirs; do
	    rm -rf $tmproot$i
	done
    done

    # Remove .db files. They should be generated.
    find new/ old/ \
	-name '*.db' \
	-exec rm -f -- "{}" \;

    message 'Checking for new files and dirs.'
    (
	cd new
	for file in $(find . ! -name .); do
	    # XXX Kinda ugly. Calls sudo very often.
	    if ! $SUDO ls ${file#.} > /dev/null 2>&1; then
		echo -n "${file#.} does not exist. " \
		    "Do you want me to install it. [Y/n] "
		read answer
		case "$answer" in
		    n*|N*)
			:
			;;
		    *)
			# Nifty huh? Makes sure the proper
			# permissions are used.
			$SUDO tar xzphf $DOWNLOADDIR/etc$BSDVERSION.tgz \
			    -C / $file
			;;
		esac
	    fi
	done
    )

    message 'creating diff'
    # Yes you do want to know about whitespace changes.
    $SUDO diff -ru old new > $DIFFFILE
    if [ -s $DIFFFILE ]; then

	/usr/bin/less $DIFFFILE

	message 'Test run. Checking for rejects.'
	$SUDO patch -s -d / -C -p1 < $DIFFFILE 2> $REJECTS
	if [ $? -ne 0 ]; then
	    warning "The patch will not apply cleanly."
	    warning "Make sure you merge the following .rej files manually."
	    cat $REJECTS
	    echo
	fi
	message 'Applying patch for real.'
	wait_for_user
	$SUDO patch -d / -b -p1 < $DIFFFILE

	# Updating databases. Yes this can be done more efficiently,
	# but I want to keep the code as readable as possible over
	# here.
	if grep '^--- old/etc/mail/aliases' $DIFFFILE > /dev/null; then
	    message 'Update to /etc/mail/aliases detected.'
	    check4rej /etc/mail/aliases
	    $SUDO /usr/bin/newaliases
	fi

	if grep '^--- old/etc/login.conf' $DIFFFILE > /dev/null; then
	    if [ -f /etc/login.conf.db ]; then
		message 'Update to /etc/login.conf detected.'
		check4rej /etc/login.conf
		$SUDO /usr/bin/cap_mkdb /etc/login.conf
	    fi
	fi

	if grep '^--- old/etc/master.passwd' $DIFFFILE > /dev/null; then
	    message 'Update to master.passwd detected.'
	    check4rej /etc/master.passwd
	    # :-)
	    while [ -e /etc/master.passwd.orig ]; do
		warning '/etc/master.passwd.orig exists.' \
		    '  Move this file out of the way so I can continue.'
		wait_for_user
	    done
	    $SUDO /usr/sbin/pwd_mkdb -p /etc/master.passwd
	fi

	if grep '^--- old/dev/MAKEDEV' $DIFFFILE > /dev/null; then
	    message 'Update to /dev/MAKEDEV detected.'
	    check4rej /dev/MAKEDEV
	    (
		cd /dev/
		$SUDO /bin/sh MAKEDEV all
	    )
	fi
    else
	message 'There was no difference!'
    fi # -s $DIFFFILE
else
    if [ -d "$TMPROOT" ]; then  # mergeslave can't work, so use mergemaster.
	echo -n "A directory named $TMPROOT still exists." \
	    " Do you want me to remove it? [y/N] "
	read answer
	case "$answer" in
            y*|Y*)
		$SUDO rm -rf $TMPROOT
		;;
            *)
		echo 'I really need a temproot.' >&2
		exit 1
		;;
	esac
    fi

    if [ ! -d "$TMPROOT" ]; then
	echo "Creating a temproot in $TMPROOT"
	if ! $SUDO mkdir -p $TMPROOT ; then
            echo "Couldn't create temproot: aborting" >&2
            exit 1
	fi
    fi

    if [ "${PACKAGES#*etc*}" != "$PACKAGES" ]; then
	echo "Extracting etc$BSDVERSION.tgz in $TMPROOT"
	$SUDO tar xzfp etc$BSDVERSION.tgz -C $TMPROOT || exit 1
    fi

    if [ "${PACKAGES#*xetc*}" != "$PACKAGES" ]; then
	echo "Extracting xetc$BSDVERSION.tgz in $TMPROOT"
	$SUDO tar xzfp xetc$BSDVERSION.tgz -C $TMPROOT || exit 1
    fi

    for i in $files; do
	rm -f $TMPROOT$i
    done
    for i in $dirs; do
	rm -rf $TMPROOT$i
    done

    $SUDO mergemaster -rat $TMPROOT || exit 1
    $SUDO mergemaster -rt  $TMPROOT || exit 1
fi # mergemaster

#
# after-script sourced from /etc/mergeslave.rc
#

afterscript

#
# optionally update bootblocks and reboot
#

cat << EOF

You now get the chance to update your bootblocks. This is
recommended especially after updating to a new release.

EOF
echo -n "Update Bootblocks? [Y/n] "
read answer
case "$answer" in
    n*|N*)
	:
	;;
    *)
        $SUDO cp /usr/mdec/boot /
        cd /usr/mdec
	set $(/bin/df /)
	rootdev=${8#/dev/}
	rootdev=${rootdev%a}
        $SUDO ./installboot /boot ./biosboot $rootdev
        ;;
esac

re_boot
