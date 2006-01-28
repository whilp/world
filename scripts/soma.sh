#!/bin/sh

# Parse somafm currently playing lists

SOMA_SITE='http://www.somafm.com'
FEED='cliqhop'
RECENT="${SOMA_SITE}/recent/?${FEED}"
MESSAGE=

HTML=$(curl -s ${RECENT} | sed -e '1,/(Now)/D' -e '/<tr>/,$D')

ARTIST=$(echo -n ${HTML} | cut -f 3 -d '>' | sed -e 's/<.*//')

ALBUM=$(echo ${HTML} | cut -f 9 -d '>' | sed -e 's/<.*//')

SONG=$(echo ${HTML} | cut -f 6 -d '>' | sed -e 's/<.*//')

if [ "$((${#ARTIST} + ${#ALBUM} + ${#SONG}))" -gt 0 ]; then
    # If the sum length of our artist, album and song information is
    # greater than zero, go ahead and process
    if [ -n "${ARTIST}" ]; then
	MESSAGE="${MESSAGE}${ARTIST} "
    fi
    if [ -n "${ALBUM}" ]; then
	MESSAGE="${MESSAGE}- [${ALBUM}] "
    fi
    if [ -n "${SONG}" ]; then
	MESSAGE="${MESSAGE}- ${SONG}"
    fi
    echo ${MESSAGE}
else
    echo "=!=> Can't connect to ${SOMA_SITE}."
fi
