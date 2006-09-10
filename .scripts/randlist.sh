#!/bin/bash

VIDDIR=/music/Video
PLAYLIST=${VIDDIR}/playlist

CUT=/usr/bin/cut
FIND=/usr/bin/find
GREP=/bin/grep
SED=/bin/sed
SEQ=/usr/bin/seq
SORT=/usr/bin/sort
RM=/bin/rm
CP=/bin/cp
MV=/bin/mv

# Find files and put them in a text file playlist; remove the
# existing playlist, if any.
# TODO: check that the file is a media file I can play
${RM} -f ${PLAYLIST}

${FIND} ${VIDDIR} -type f -name "*" > ${PLAYLIST}

# DEBUG
${CP} ${PLAYLIST} ${PLAYLIST}.bak

# Get a head count of the files to play.
ITEMS=$(wc -l ${PLAYLIST} | ${CUT} -d ' ' -f 1)

for i in $(${SEQ} 1 ${ITEMS}); do
    # Generate a randomish index number
    NUMBER=$(( RANDOM % ITEMS ))
    # Keep generating it until we have a number we haven't had
    # before.
    # while [ $(${GREP} -c "^${NUMBER}:::" ${PLAYLIST}) -gt 0 ]; do
    #     NUMBER=$(( RANDOM % ITEMS ))
    # done
    # Add the randomish index number to the front of every line of
    # the playlist.
    ${SED} "${i}s/^/${NUMBER}:::/" ${PLAYLIST} > ${PLAYLIST}.new
done

exit
${RM} ${PLAYLIST}

# Sort the playlist based on those randomish numbers, hopefully
# producing a convincingly random playlist.
${SORT} -rn ${PLAYLIST}.new > ${PLAYLIST}

# Remove the leading index numbers.
${SED} -i 's/^.*::://' ${PLAYLIST}

# while :; do
# find . -type f >! playlist && /usr/bin/mplayer -nobps -display localhost.localdomain:0 -fs -zoom -playlist playlist -shuffle
# done
