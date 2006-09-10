#!/bin/sh
##################  BEGIN HEADERS
# Filename	: $HOME/bin/maildirs
# Use		: finds mail folders within a Maildir that are unused and
#		  deletes them
# Author	: Will Maier <willmaier@ml1.net>
# Updated	: 2005.08.19 09:45:52 -0500
# Copyright	: Copyright (c) 2005 Will Maier
# License	: Expat; see <http://www.opensource.org/licenses/mit-license.php>
##################  END HEADERS

MAIL=/usr/bin/mail

MAILDIR=$HOME/.maildir

# find folders
DIRS=$(find $MAILDIR -type d -name "*cur")
#DIRS=$(find $MAILDIR -type d -regex '.*cur$')
MESSAGE=''


# if cur AND new are empty, delete parent, unless parent is on a special list
for DIR in $DIRS; do
    PARENT=$(dirname $DIR)
    CUR_RECORDS=$(ls $PARENT/cur | wc -l)
    TMP_RECORDS=$(ls $PARENT/tmp | wc -l)
    NEW_RECORDS=$(ls $PARENT/new | wc -l)
    MAILSUM=$((NEW_RECORDS + TMP_RECORDS + CUR_RECORDS))
    if [ "${MAILSUM}" -eq "0" ]; then
	case $(basename ${PARENT}) in
	    OpenBSD-cvs|Questionable|Draft*|Inbox|News)
	    # I seem to need something harmless here...
	    ;;
	    *)
	    MESSAGE="${MESSAGE} ${PARENT}"
	    rm -rf $PARENT
	    ;;
	esac
    fi
done 

if [ -n "${MESSAGE}" ]; then
    MAILMSG="The following mailboxes were deleted at $(date):"
    for i in ${MESSAGE}; do
	MAILMSG="   ${MAILMSG}\n$i"
    done
    echo $MAILMSG | ${MAIL} -s "Reaping empty mailboxes" will
fi
