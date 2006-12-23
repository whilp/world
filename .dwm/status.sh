#!/bin/sh

FIFO="${HOME}/.dwm/fifo"
MPD_HOST="localhost"
MPD_PORT=6600
NAME=$(hostname -s)

#exec > ${FIFO}

i=0
MAX_LEN=50
LB=1
RB=${MAX_LEN}
SCROLL_SPEED=2
while :; do
    DATE=$(date "+%a %d %b %H:%M:%S UTC%z %Y")

    OMPD="${MPD}"
    # Only check MPD every 45 seconds.
    if [ "${i}" -gt 45 -o ! "${OMPD}" ]; then
        MPD_RESP=$(echo 'currentsong' | nc ${MPD_HOST} ${MPD_PORT})
        MPD_RESP=$(echo ${MPD_RESP} | sed -e 's/. Title: \(.*\) Artist: \(.*\) Album: \(.*\) Pos:.*/\2 (\3) - \1/')
        print 'Checking MPD'
    else
        MPD_RESP="${OMPD}"
    fi

    LEN=$(echo ${MPD_RESP} | wc -c)
    if [ "${MPD_RESP}" = "${OMPD}" -a "${LEN}" -gt "${MAX_LEN}" ]; then
        # We need to truncate and scroll.
        if [ "${LB}" -gt "${MAX_LEN}" ]; then
            LB=1
            RB=${MAX_LEN}
        else
            LB=$((LB + SCROLL_SPEED))
            RB=$((RB + SCROLL_SPEED))
        fi
        MPD_OUT="$(echo ${MPD_RESP} | cut -c ${LB}-${RB})..."
        NEW_LEN=$(echo ${MPD_OUT} | wc -c)
        if [ "${NEW_LEN}" -lt "${MAX_LEN}" ]; then
            # Append whitespace to fill out the block.
            j=0
            while [ "$j" -lt "$((MAX_LEN - NEW_LEN))" ]; do
                MPD="${MPD_OUT} "
            done
        fi
    fi

    echo "[NP: ${MPD}][${NAME}][${DATE}]"

    i=$((i+1))
    sleep 1
done
