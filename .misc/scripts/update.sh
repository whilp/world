#!/bin/sh
##################  BEGIN HEADERS
# Filename	: update.sh
# Use		: Performs an update of system CVS trees for OpenBSD
# Author	: Will Maier <willmaier@ml1.net>
# Version	: $Revision: 1.1 $
# Updated	: $Date: 2005/11/29 18:45:48 $
# Vim		: :vim: set ft=sh:
# CVS		: $Id: update.sh,v 1.1 2005/11/29 18:45:48 will Exp $
# Copyright	: Copyright (c) 2005 Will Maier
# License	: Expat; see <http://www.opensource.org/licenses/mit-license.php>
##################  END HEADERS

# Binaries
CVS=/usr/bin/cvs
CAT=/bin/cat
DATE=/bin/date

# Variables
CVSROOT=${CVSROOT:-anoncvs@mirror.sg.depaul.edu:/cvs}
SRCDIR=/usr/src
PORTSDIR=/usr/ports
XDIR=/usr/XF4
UPDATEDIRS="${SRCDIR} ${PORTSDIR}"	    # Add XDIR if you want it updated
CVSTAG=OPENBSD_3_8
DATE=$(date)


for DIR in ${UPDATEDIRS}; do
    cd ${DIR} && ${CVS} -q up -r${CVSTAG} -Pd 
    if [ "$?" ]; then
	# CVS update failed; report infos
	echo "Update of ${SRCDIR} failed on ${DATE} using the following information:"
	echo "CVSTAG: ${CVSTAG}"
	echo "UPDATEDIRS: ${UPATEDIRS}"
	echo "CVSROOT: ${CVSROOT}"
	return 1
    fi
done

echo "Update completed on ${DATE} using the following information:"
echo "UPDATEDIRS: ${UPDATEDIRS}"
echo "CVSROOT: ${CVSROOT}"
echo "CVSTAG: ${CVSTAG}"

echo ""

echo "The kernel and userland should now be rebuilt:"
cat<<EOF
    # Rebuild kernel
    cd /usr/src/sys/arch/i386/conf
    vi GENERIC
    /usr/sbin/config GENERIC
    cd /usr/src/sys/arch/i386/compile/GENERIC
    make clean && make depend && make
    # Reboot using new kernel
    cd /usr/src/sys/arch/i386/compile/GENERIC
    cp /bsd /bsd.old          # Save an old copy of your kernel
    cp bsd /bsd               # Copy the new kernel into place
    reboot
    # Rebuild binaries
    cd /usr/src
    rm -r /usr/obj/*
    make obj && make build
EOF
