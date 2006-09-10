#!/bin/sh

# Makes a little 'M' appear in screen's hardstatus when I've got new
# mail.

# Programs used.
GREP=/usr/bin/egrep
WC=/usr/bin/wc
FIND=/usr/bin/find
CUT=/usr/bin/cut
SED=/usr/bin/sed

# Settings.
MAILDIR=$HOME/Maildir
BOXES=

# Check that there's a maildir here; not much point running
# otherwise.
if [ ! -d "${MAILDIR}" ]; then
    return 1
fi

NUMBER="$(${FIND} ${MAILDIR} -path "*new/[^/]*" -type f | \
    ${WC} -l | \
    ${SED} 's/ //g')"
if [ "${NUMBER}" -gt 0 ]; then
    echo ${NUMBER}
fi
