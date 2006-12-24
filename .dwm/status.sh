#!/bin/sh

# Functions.
checkmpd () {
    MPD_RESP=$(echo 'currentsong' | nc ${MPD_HOST} ${MPD_PORT})
    STRING=$(echo ${MPD_RESP} | sed -e 's/^.* Artist: \(.*\) Album: \(.*\) Title: \(.*\) Pos:.*$/\1 (\2) - \3/')
    echo ${STRING}
}
checkdate () {
    echo $(date "+%a %d %b %H:%M %Z %Y")
}

# Options.
FIFO="${HOME}/.dwm/fifo"
MPD_HOST=localhost
MPD_PORT=6600
NAME=$(hostname -s)

# Redirect stdout.
exec > ~/.dwm/fifo

# Seed MPD.
MPD=$(checkmpd)
MPD_LEN=$(echo ${MPD} | wc -c)

# Seed date.
DATE=$(checkdate)

# For NP section.
MAX_LEN=35
L=0
R=${MAX_LEN}

# Main loop.
i=0
while :; do
    # Increments.
    L=$((L + 1))
    R=$((R + 1))
    i=$((i + 1))

    MPD_OUT=$(echo ${MPD} | cut -c ${L}-${R})

    # Save old MPD string.
    if [ "$((R + 1))" -eq "${MPD_LEN}" ]; then
        OMPD=${MPD}
    fi

    # Check date.
    if [ $i -ge 60 ]; then
        DATE=$(checkdate)
        i=0
    fi

    # Reset MPD.
    if [ "$((R + 1))" -eq "${MPD_LEN}" ]; then
        L=0
        R="${MAX_LEN}"
        MPD=$(checkmpd)
        MPD_LEN=$(echo ${MPD} | wc -c)
    fi

    # Pause for a bit if we're at the end or beginning of the NP
    # string.
    if [ "${L}" -le 1 ]; then
        SLEEP=2
    else
        SLEEP=.5
    fi

    echo "[$MPD_OUT][${DATE}]"

    sleep ${SLEEP}
done
