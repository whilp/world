#!/usr/bin/env sh
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
DIRS=`find $MAILDIR -type d -regex '.*cur$'`

# if cur AND new are empty, delete parent, unless parent is on a special list
for DIR in $DIRS; do
    PARENT=`dirname $DIR`
    CUR_RECORDS=`ls $DIR | wc -l`
    NEW_RECORDS=`ls $PARENT/new | wc -l`

    if [[ $CUR_RECORDS < "1" && $TMP_RECORDS < "1" ]]; then
	case `basename $PARENT` in
	    Draft*|Inbox|News)
	    shift
	    ;;
	    *)
	    echo "Deleting $PARENT..."
	    rm -rf $PARENT
	    ;;
	esac
    fi
done

exit 0
