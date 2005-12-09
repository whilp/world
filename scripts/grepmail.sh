#!/bin/sh

FIND=/usr/local/bin/gfind
EGREP=/usr/bin/egrep
SED=/usr/bin/sed
XARGS=/usr/bin/xargs
LN=/bin/ln
MKDIR=/bin/mkdir
BASENAME=/usr/bin/basename

PATTERN=$@
MAILDIRS="${HOME}/Maildir"
VFOLDER="Search"
VFOLDERPATH="${MAILDIRS}/${VFOLDER}/cur"
# FOLDERS=`awk '{print $2}' $HOME/.mutt/mailboxes | sed 's/^"\=\(.*\)\"$/\1 /' \
# | xargs echo`
# FOLDERS=$(${FIND} ${MAILDIRS} -type d | \
#     ${EGREP} -v 'cur$|tmp$|new$|Sent$|Maildir$|Hack$|Work$|Personal$' | \
#     ${SED} 's/.*\/Maildir\///' | \
#     ${XARGS} -I% echo '%')

TEST="CAE"
TEST2="D*"
# Works:
# '*' and "*"

FOLDERS=''
echo "LOOP ONE"
time for i in ${MAILDIRS}/*; do
    MAYBEDIR=$(${BASENAME} $i)
    case ${MAYBEDIR} in
	drop.log|marker)
	# Files; we don't care about these
	;;
	${TEST}|${TEST2})
	# Optional excludes
	;;
	*)
	FOLDERS="${FOLDERS} ${MAYBEDIR}"
	;;
    esac
done

echo "BLOCKING: ${TEST}"
echo "RESULT: ${FOLDERS}"

exit

# Make sure the vfolder already exists
echo "LOOP TWO"
time for DIR in cur tmp new; do
    if [ ! -d ${MAILDIRS}/${VFOLDER}/${DIR} ]; then
	${MKDIR} -p ${MAILDIRS}/${VFOLDER}/${DIR}
    fi
done

#rm ${VFOLDERPATH}/*
echo "Remove ${VFOLDERPATH}/*"

echo "LOOP THREE"
time for FOLDER in ${FOLDERS}; do
    echo "LOOP THREE; FOLDER ${FOLDER}"
    time for RESULT in $(${EGREP} -lR "${PATTERN}" ${MAILDIRS}/${FOLDER}); do
	echo "${LN} -s ${RESULT} $VFOLDERPATH/."
	#echo "Link ${RESULT} to ${VFOLDERPATH}/."
    done
done
