#!/bin/sh
##################  BEGIN HEADERS
# Filename	: $HOME/bin/portup.sh
# Use		: manage the port update process
# Author	: Will Maier <willmaier@ml1.net>
# Started	: 2005.09.28
# Updated	: 2005.10.05 16:45:00 -0500
# Copyright	: Copyright (c) 2005 Will Maier
# License	: Expat; see <http://www.opensource.org/licenses/mit-license.php>
##################  END HEADERS

PORTSNAP=/usr/local/sbin/portsnap
PORTSDB=/usr/local/sbin/portsdb
PORTVERSION=/usr/local/sbin/portversion
PORTAUDIT=/usr/local/sbin/portaudit
GREP=/usr/bin/grep
SED=/usr/bin/sed

NEEDUPDATE='0'

echo "====> Downloading updates to the ports tree."

# Check to see if there are new versions of the ports available
${PORTSNAP} fetch | tail -1 |\
while read LINE; do
    echo ${LINE}
    if [ -z "$(echo ${LINE} | ${GREP} 'No updates needed')" ]; then
	NEEDUPDATE=1
    else
	echo "====> Ports tree up to date. Verifying installed packages are fresh and secure."
    fi
done

if [ "${NEEDUPDATE}" -eq "1" ]; then
    # If we got updated ports above, extract those updates
    echo "====> Extracting updates."
fi

SNAPU=$(${PORTSNAP} update)

if [ -n "$(echo ${SNAPU} | ${GREP} '(MOV|UPDATING)')" ]; then
    # If, in extracting the new ports, we also got a new /u/ports/UPDATING
    # file, prompt the user to read it
    echo "====> Please read /usr/ports/UPDATING and /usr/ports/MOVED"
    echo "====> before running portmanager."
fi

echo "====> Updating packages databse."
${PORTSDB} -Fu 2> /dev/null

    
echo "====> Comparing available and installed packages."
VERSION=$(${PORTVERSION} -l "<")
    
if [ -z "${VERSION}" ]; then
    echo "====> All installed ports are up to date."
else
    echo "====> The following ports need to be upgraded. Please run"
    echo '====> `portmanager -u`' "to compile the new versions."
    echo ${VERSION} |\
    while read LINE; do
	echo ${LINE} | ${SED} -E 's/[^a-z0-9-]//g'
    done
fi

echo "====> Fetching new database of port security vulnerabilities."

# Check ports for security vulnerabilities
${PORTAUDIT} -Fdq 2>&1 > /dev/null

echo "====> Checking ports tree for known vulnerabilities."
${PORTAUDIT} -a
