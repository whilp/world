#!/bin/sh

SECONDS=$(date '+%S')
if [ -x "$(which tunes)" -a "$((SECONDS % 7))" -eq "0" ]; then
    [ "$(tunes query | sed -e '/\(playing\|paused\)/!d')" ] && \
        PLAYING="$(tunes query)"
        # PLAYING="$(tunes query | sed -e '1!d' -e 's/[^a-zA-Z '?.\/\\_+-]/_/g')"
    [ "${PLAYING}" ] && CONTENTS="${CONTENTS}[MPD: ${PLAYING}]"
    UPTIME="$(uptime | sed -e 's/.*up //' -e 's/, [0-9 ]\+ users.*//')"
    CONTENTS="${CONTENTS}[UP: ${UPTIME}]"
fi
CONTENTS="${CONTENTS}[$HOSTNAME]"
CONTENTS="${CONTENTS}[$(date)]"
echo $CONTENTS
