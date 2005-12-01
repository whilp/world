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


MAILDIR=$HOME/Maildir

# find folders
DIRS=$(find $MAILDIR -type d -name "*cur")
#DIRS=$(find $MAILDIR -type d -regex '.*cur$')
echo $DIRS
exit

# if cur AND new are empty, delete parent, unless parent is on a special list
for DIR in $DIRS; do
    echo $DIR
    PARENT=$(dirname $DIR)
    CUR_RECORDS=$(ls $PARENT/cur | wc -l)
    TMP_RECORDS=$(ls $PARENT/tmp | wc -l)
    NEW_RECORDS=$(ls $PARENT/new | wc -l)

    if [ $(( CUR_RECORDS + NEW_RECORDS + TMP_RECORDS )) -eq "0" ]; then
    #if [[ $CUR_RECORDS < "1" && $NEW_RECORDS < "1" ]]; then
	case $(basename $PARENT) in
	    Questionable|Draft*|Inbox|News)
	    break
	    ;;
	    *)
	    echo "Deleting $PARENT..."
	    #rm -rf $PARENT
	    ;;
	esac
    fi
done

exit 0
