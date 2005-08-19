#!/usr/bin/env sh

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
	    echo "Recommending to delete $PARENT"
	    rm -rf $PARENT
	    ;;
	esac
    fi
done

exit 0
