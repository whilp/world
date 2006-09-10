#!/bin/sh

if [ -x "$(which tunes)" -a "$(tunes query | sed -e '/\(playing\|paused\)/!d')" ]; then 
    # Need to sanitize the output, too; Sigur Ros made that
    # obvious enough...
    PLAYING="$(tunes query | sed -e '1!d')"
    # PLAYING="$(tunes query | sed -e '1!d' -e 's/[^a-zA-Z ?.\/\\_+-]/_/g')"
else
    PLAYING=''
fi
[ "${PLAYING}" ] && CONTENTS="${CONTENTS}[MPD: ${PLAYING}]"
UPTIME="$(uptime | sed -e 's/.*up //' -e 's/, [0-9 ]\+ users.*//')"
CONTENTS="${CONTENTS}[UP: ${UPTIME}]"
CONTENTS="${CONTENTS}[$HOSTNAME]"
CONTENTS="${CONTENTS}[$(date)]"
echo $CONTENTS
