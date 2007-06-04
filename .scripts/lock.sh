#!/bin/sh

MAX=220
LOCKFILE="~/.vol-lock"
OUTPUT="outputs.headphones"
SLEEP=.1
AMOUNT=0
INCREMENT=3

if [ -e "${LOCKFILE}" ]; then
    exit 1
fi

calc () {
    cat <<EOF | bc -l
    scale=0
    $*
EOF
}

changevol () {
    mixerctl "${OUTPUT}=$1" >/dev/null
}

mute () {
    mixerctl "${OUTPUT}.mute=$1" >/dev/null
}

checkvol () {
    VOL=$(mixerctl "${OUTPUT}" | cut -d '=' -f 2)
    LVOL=$(echo ${VOL} | cut -d ',' -f 1)
    RVOL=$(echo ${VOL} | cut -d ',' -f 2)
    echo $(calc "(${LVOL} + ${RVOL})/2")
}

fadeout () {
    NEWVOL=$(checkvol)
    OAMOUNT=${AMOUNT}
    while [ "${NEWVOL}" -gt 0 ]; do
        AMOUNT=$((AMOUNT + INCREMENT))
        NEWVOL=$((NEWVOL - AMOUNT))
        changevol ${NEWVOL}
        sleep "${SLEEP}"
    done
    mute on
    AMOUNT=${OAMOUNT}
}

fadein () {
    NEWVOL=$(checkvol)
    OAMOUNT=${AMOUNT}
    mute off
    while [ "${NEWVOL}" -lt ${MAX} ]; do
        AMOUNT=$((AMOUNT + INCREMENT))
        NEWVOL=$((NEWVOL + AMOUNT))
        changevol ${NEWVOL}
        sleep "${SLEEP}"
    done
    AMOUNT=${OAMOUNT}
}

fadeout && slock && fadein
