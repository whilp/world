#!/bin/sh
##################  BEGIN HEADERS
# Filename	: $HOME/bin/portup.sh
# Use		: manage the port update process
# Author	: Will Maier <willmaier@ml1.net>
# Started	: 2005.09.28
# Updated	: 2005.09.30 18:51:26 -0500
# Copyright	: Copyright (c) 2005 Will Maier
# License	: Expat; see <http://www.opensource.org/licenses/mit-license.php>
##################  END HEADERS

PORTSNAP=/usr/local/sbin/portsnap
PORTSDB=/usr/local/sbin/portsdb
PORTVERSION=/usr/local/sbin/portversion
PORTAUDIT=/usr/local/sbin/portaudit
GREP=/usr/bin/grep
SED=/usr/bin/sed

echo "====> Checking for updates to the ports tree."

# Check to see if there are new versions of the ports available
SNAPF=$(${PORTSNAP} fetch | tail -1)

if [ -z "$(echo $SNAPF | ${GREP} 'No updates needed')" ]; then
    # If we got updated ports above, extract those updates
    echo "====> Downloading..."
    SNAPU=$(${PORTSNAP} update)

    if [ -n "$(echo ${SNAPU} | ${GREP} 'UPDATING')" ]; then
	# If, in extracting the new ports, we also got a new /u/ports/UPDATING
	# file, prompt the user to read it
	echo "====> Please read /usr/ports/UPDATING before running portupgrade."
    fi
    ${PORTSDB} -Fu 2> /dev/null
    
    VERSION=$(${PORTVERSION} -v -l "<")
    
    if [ -z "${VERSION}" ]; then
	echo "====> All installed ports are up to date."
    else
	echo "====> The following ports need to be upgraded. Please run"
	echo '====> `portupgrade -varR`' "to compile the new versions."
	for line in ${VERSION}; do
	    # Output of portversion looks like:
	    # keychain-2.5.4.1 < needs updating (port has 2.5.5)
	    PACKAGE="$(echo ${line} | ${SED} 's/^\([^ ]*\) .*/\1/')"
	    AVAIL="$(echo ${line} | ${SED} 's/.*(port has \([^ ]*\))/\1/')"
	    echo "      ${PACKAGE} -> ${AVAIL}"
	done
	echo "====> Ports tree up to date."
    fi
else
    echo "====> Ports tree up to date."
fi

echo "====> Fetching new database of port security vulnerabilities."

# Check ports for security vulnerabilities
${PORTAUDIT} -Fd 2>&1 > /dev/null

echo "====> Checking ports tree for known vulnerabilities."
${PORTAUDIT} -a
