#!/bin/sh

TARGET=$1
DURATION=
INTERVAL=3
VERBOSE=1
STATE=
CURRENT=

while :; do
    DATE="$(date '+%Y.%m.%d %H:%M:%S')"
    if [ "${STATE}" ]; then
        OLDSTATE="${STATE}"
    else
        OLDSTATE=-1
    fi
    
    ping -c 1 -w 1 "${TARGET}" >/dev/null 2>&1 && \
        STATE="UP" ||
        STATE="DOWN"

    if [ "${STATE}" = "${OLDSTATE}" ]; then
        [ "${VERBOSE}" -gt 2 ] && echo "[${DATE}] Status for ${TARGET} unchanged."
    else
        echo "[${DATE}] ${TARGET} -> ${STATE}."
    fi
    sleep "${INTERVAL}"
done
