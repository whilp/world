#!/bin/sh

# Functions.
checkdate () {
    echo $(date "+%a %d %b %H:%M %Z %Y")
}
len () {
    echo -n "$1" | wc -c
}

# Settings.
FIFO="${HOME}/.dwm/fifo"
LOCK="${HOME}/.dwm/.status-lock"
DATE_INTERVAL=100
SLEEP=.5

if [ -e "${LOCK}" -a ! "x$1" = "x-f" ]; then
    exit 1
else
    echo $$ >| ${LOCK} 2>/dev/null
    [ $? -eq 0 ] || exit 1
fi

# Date counter.
D=0

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

    # Print the final status message.
    echo "${OUT}"

    # Sleep. 
    sleep ${SLEEP}
done
