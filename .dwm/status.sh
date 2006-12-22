#!/bin/sh

FIFO="${HOME}/.dwm/fifo"
MPD_HOST="localhost"
MPD_PORT=6600

exec > ${FIFO}

i=0
while :; do
    DATE=$(date "+%a %d %b %H:%M:%S UTC%z %Y")
    NAME=$(hostname -s)

    # Only check MPD every 45 seconds.
    OMPD="${MPD}"
    if [ "${i}" -gt 45 -o ! "${OMPD}" ]; then
        MPD=$(echo 'currentsong' | nc ${MPD_HOST} ${MPD_PORT})
        MPD=$(echo $MPD | sed -e 's/.*Title: \(.*\) Artist: \(.*\) Album: \(.*\) Pos:.*/\2 (\3) - \1/')
    else
        MPD="${OMPD}"
    fi

    echo "[NP: ${MPD}][${NAME}][${DATE}]"

    i=$((i+1))
    sleep 1
done
