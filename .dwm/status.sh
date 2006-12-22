#!/bin/sh

FIFO="${HOME}/.dwm/fifo"
MPD_HOST="localhost"
MPD_PORT=6600
NAME=$(hostname -s)

exec > ${FIFO}

i=0
MAX_LEN=50
LB=1
RB=${MAX_LEN}
SCROLL_SPEED=2
while :; do
    DATE=$(date "+%a %d %b %H:%M:%S UTC%z %Y")

    # Only check MPD every 45 seconds.
    if [ "${i}" -gt 45 -o ! "${OMPD}" ]; then
        MPD=$(echo 'currentsong' | nc ${MPD_HOST} ${MPD_PORT})
        MPD=$(echo $MPD | sed -e 's/.*Title: \(.*\) Artist: \(.*\) Album: \(.*\) Pos:.*/\2 (\3) - \1/')
    else
        MPD="${OMPD}"
    fi
    OMPD="${MPD}"

    LEN=$(echo ${MPD} | wc -c)
    if [ "${MPD}" = "${OMPD}" -a "${LEN}" -gt "${MAX_LEN}" ]; then
        # We need to truncate and scroll.
        if [ "${LB}" -gt "${MAX_LEN}" ]; then
            LB=1
            RB=${MAX_LEN}
        else
            LB=$((LB + SCROLL_SPEED))
            RB=$((RB + SCROLL_SPEED))
        fi
        MPD="$(echo ${MPD} | cut -c ${LB}-${RB})..."
    fi

    echo "[NP: ${MPD}][${NAME}][${DATE}]"

    i=$((i+1))
    sleep 1
done
