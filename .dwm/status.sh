#!/bin/sh

# Functions.
checkdate () {
    echo $(date "+%a %d %b %H:%M %Z %Y")
}
checkmpdstatus() {
    I=$(echo -e "status\nclose" | nc ${MPD_HOST} ${MPD_PORT} | grep -E 'state:')
    S=$(echo ${I} | sed -e 's/state: //')
    echo ${S}
}
checkmpd () {
    I=$(echo -e "currentsong\nclose" | nc ${MPD_HOST} ${MPD_PORT} | grep -E '^(Album|Artist|Title):' | sort)
    O=$(echo ${I} | sed -e 's/^Album: \(.*\) Artist: \(.*\) Title: \(.*\)$/\2 (\1) - \3/')     

    if [ "$(echo ${O} | grep '^OK.*OK$')" ]; then
        O=
    fi

    echo ${O}
}
len () {
    echo -n "$1" | wc -c
}

# Settings.
FIFO="${HOME}/.dwm/fifo"
LOCK="${HOME}/.dwm/.status-lock"
DATE_INTERVAL=100
MPD_INTERVAL=10
SLEEP=.5
MPD_HOST=localhost
MPD_PORT=6600
MPD_NOSONG=
CHECK_MPD=1
SCR_DELIM=" // "

if [ -e "${LOCK}" -a ! "x$1" = "x-f" ]; then
    exit 1
else
    echo $$ >| ${LOCK} 2>/dev/null
    [ $? -eq 0 ] || exit 1
fi

# Date counter.
D=0

# MPD counters.
MPD_MAX=40
MPD_BAD=0
M=0

# Redirect stdout to the fifo dwm's listening to.
exec > ${FIFO}

while :; do
    # Date stuff.
    if [ -z "${DATE_OUT}" -o "${D}" -ge "${DATE_INTERVAL}" ]; then
        # check date
        DATE_OUT="$(checkdate)"
        D=0
    else
        D="$(($D + 1))"
    fi
    OUT="[${DATE_OUT}]"

    # MPD stuff.
    if [ -z "${MPD_IN}" -o "${M}" -ge "${MPD_INTERVAL}" ]; then
        if [ "${MPD_BAD}" -eq 0 -o "${MPD_BAD}" -ge 30 ]; then
            MPD_BAD=0

            # Check status and decide whether to poll currentsong.
            MPD_STATUS=$(checkmpdstatus)

            if [ "${MPD_STATUS}" = "play" ]; then
                MPD_IN=$(checkmpd)
            elif [ "${MPD_STATUS}" = "pause" ]; then
                MPD_IN="$(checkmpd) <paused>"
            else
                MPD_IN="${MPD_NOSONG}"
            fi
        fi
        M=0
    else
        M=$(($M + 1))
    fi

    if [ -z "${MPD_IN}" ]; then
        # We've gotten two bad checks from MPD in a row.
        MPD_BAD=$(($MPD_BAD + 1))
    fi
    # Handle scrolling if necessary.
    if [ -n "${MPD_IN}" ]; then

        # Since updates to MPD_IN may happen while we're scrolling,
        # check to see if MPD_IN has changed since we last saw it.
        if [ ! "${MPD_IN}" = "${MPD_LAST_IN}" ]; then
            L=1
            R=${MPD_MAX}
            MPD_LAST_IN=${MPD_IN}

            # Calculate length and length with delimiter. This is
            # the only time that these values need to be updated.
            MPD_LEN="$(len "${MPD_IN}")"
            MPD_DLEN="$(len "${MPD_IN}${SCR_DELIM}")"
        fi

        # Scroll if (undelimited) length exceeds max.
        if [ "${MPD_LEN}" -gt "${MPD_MAX}" ]; then

            MPD_OUT="$(echo "${MPD_IN}${SCR_DELIM}${MPD_IN}" | cut -c "${L}-${R}")"

            if [ "${L}" -eq "${MPD_DLEN}" ]; then
                # We're at the end of the string; reset counters.
                L=1
                R=${MPD_MAX}
            else
                # We're not at the end of the string; increment counters.
                L=$(($L + 1))
                R=$(($R + 1))
            fi

        else
            MPD_OUT=${MPD_IN}
        fi
    fi
    # Add to OUT only if we have anything to say.
    [ "${MPD_OUT}" ] && OUT="[${MPD_OUT}]${OUT}"

    # Print the final status message.
    echo "${OUT}"

    # Sleep. 
    sleep ${SLEEP}
done
