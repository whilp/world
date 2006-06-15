#!/bin/sh

# TODO:
# Arg parsing
# Input sanitization
# Check ssh availability?
# Comments!

# From rader's /usr/local/etc/pinger:
# usage:  pinger [args] host [... hostN]
#         pinger [args] -r X-Y basename
# -nb            no bell
# -c             count (default is 5 packets)
# -w             deadline (default is 1 second)
# -s             packetsize (default is 100 bytes)
# -t X           % packet loss threshold (default is > 60)
# -r X-Y         for hosts X through Y
# --once         ping once and then exit
# --domain name  append "name" to host names

TARGET=$1       # Should fail if nothing's specified
DURATION=
INTERVAL=20
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
