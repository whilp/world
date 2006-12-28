#!/bin/sh

# Functions.
checkdate () {
    echo $(date "+%a %d %b %H:%M %Z %Y")
}
checkmpd () {
    I=$(echo 'currentsong' | nc -w 1 ${MPD_HOST} ${MPD_PORT})
    O=$(echo ${I} | sed -e 's/^.* Artist: \(.*\) Album: \(.*\) Title: \(.*\) Pos:.*$/\1 (\2) - \3/')     

    if [ "$(echo ${O} | grep '^OK.*OK$')" ]; then
        O=
    fi

    echo ${O}
}
len () {
    echo $1 | wc -c
}

# Settings.
FIFO="${HOME}/.dwm/fifo"
DATE_INTERVAL=100
MPD_INTERVAL=100
SLEEP=.5
MPD_HOST=localhost
MPD_PORT=6600
CHECK_MPD=1

# Date counter.
D=0

# MPD counters.
MPD_MAX=30
MPD_BAD=0
L=0
R=${MPD_MAX}
M=0

# Redirect stdout to the fifo dwm's listening to.
exec > ${FIFO}

while :; do
    # Date stuff.
    if [ -z "${DATE_OUT}" -o "$D" -ge "${DATE_INTERVAL}" ]; then
        # check date
        DATE_OUT="$(checkdate)"
        D=0
    else
        D="$(($D + 1))"
    fi
    OUT="[${DATE_OUT}]"

    # MPD stuff.
    if [ -z "${MPD_IN}" -o "${L}" -eq "0" ]; then
        if [ "${MPD_BAD}" -eq 0 -o "${MPD_BAD}" -ge 30 ]; then
            MPD_BAD=0
            MPD_IN=$(checkmpd)
        fi
    fi
    if [ -z "${MPD_IN}" ]; then
        # We've gotten two bad checks from MPD in a row.
        MPD_BAD=$(($MPD_BAD + 1))
    fi
    # Handle scrolling if necessary.
    if [ -n "${MPD_IN}" ]; then
        MPD_LEN="$(len "${MPD_IN}")"
        if [ -z "${MPD_PAUSE}" -a "$((MPD_LEN - 1))" -ge "${MPD_MAX}" ]; then
            # Scroll.
            L=$(($L + 1))
            R=$(($R + 1))
            MPD_OUT="$(echo ${MPD_IN} | cut -c "${L}-${R}")"
            if [ "${L}" -le "1" ]; then
                # We're at the beginning of the string.
                MPD_PAUSE="${MPD_OUT}"
            elif [ "$(($R + 1))" -ge "${MPD_LEN}" ]; then
                # We're at the end of the string.
                L=0
                R=${MPD_MAX}
                MPD_PAUSE="${MPD_OUT}"
                MPD_IN=
            fi
        else
            MPD_OUT="${MPD_PAUSE:-${MPD_IN}}"
            MPD_PAUSE=
        fi
    fi
    # Add to OUT only if we have anything to say.
    [ "${MPD_OUT}" ] && OUT="[${MPD_OUT}]${OUT}"

    # Print the final status message.
    echo "${OUT}"

    # Sleep. 
    sleep ${SLEEP}
done
