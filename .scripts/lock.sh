#!/bin/sh

MAX=220
LOCKFILE="~/.vol-lock"
OUTPUT="outputs.headphones"
SLEEP=.1
AMOUNT=20
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
    set -x
    NEWVOL=$(checkvol)
    AMT=0
    while [ "${NEWVOL}" -gt 0 ]; do
        [ ${AMT} -lt ${AMOUNT} ] && AMT=$((AMT + 1))
        #[ ${AMOUNT} -lt 10 ] && AMOUNT=$((AMOUNT + INCREMENT))
        NEWVOL=$((NEWVOL - AMT))
        changevol ${NEWVOL}
        sleep "${SLEEP}"
    done
    mute on
}

fadein () {
    set -x
    NEWVOL=$(checkvol)
    AMT=${AMOUNT}
    mute off
    while [ "${NEWVOL}" -lt ${MAX} ]; do
        [ ${AMT} -gt 1 ] && AMT=$((AMT - 1))
        #[ ${AMOUNT} -lt 10 ] && AMOUNT=$((AMOUNT + INCREMENT))
        NEWVOL=$((NEWVOL + AMT))
        changevol ${NEWVOL}
        sleep "${SLEEP}"
    done
}

fadeout &
sleep .2
slock 
sleep .5
fadein &
