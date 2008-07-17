#!/bin/sh

sysctln () {
    sysctl -n $* | sed -e 's/ .*$//'
}

calcl () {
    cat <<EOF | bc -l
    scale=2
	$*
EOF
}

calc () {
    cat <<EOF | bc
	$*
EOF
}

REMAINING=$(sysctln hw.sensors.acpibat0.watthour3)
FULL=$(sysctln hw.sensors.acpibat0.watthour0)
LOW=$(sysctln hw.sensors.acpibat0.watthour2)
RATE=$(sysctln hw.sensors.acpibat0.raw1)

MINUTES=$(calc "60 * ((1000 * ${REMAINING})/${RATE})")

if [ "${MINUTES}" -gt 60 ]; then
    HOURS=$(calc "${MINUTES} / 60")
    MINUTES=$(calc "${MINUTES} % 60")
    if [ "${MINUTES}" -eq 0 ]; then
        printf "%dh" "${HOURS}"
    else
        printf "%dh %dm" "${HOURS}" "${MINUTES}"
    fi
else
    printf "%dm" "${MINUTES}"
fi


PERCENT=$(calcl "100 * ${REMAINING}/${FULL}")

printf " (%.02f%%)" "${PERCENT}"

if [ $(calcl "${REMAINING} < ${LOW}") -eq 1 ]; then
    printf "  LOW"
fi

printf "\n"
