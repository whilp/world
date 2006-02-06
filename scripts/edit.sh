#!/bin/sh

notify () {
    if [ ${VERBOSE} -ge $1 ]; then
	echo "===> $2"
    fi
}
warn () {
    if [ ${VERBOSE} -ge $1 ]; then
	echo "=!=> $2"
    fi
}

FILE=$1
FILEPATH="${FILE%/*}"
FILENAME="${FILE##*/}"
if [ "${FILEPATH}" -eq "${FILE}" ]; then
    FILEPATH=${PWD}
fi
RCSDIR="${FILEPATH}/RCS"
RCSFILE="${RCSDIR}/${FILENAME},v"

if [ ! -f "${RCSFILE}" ]; then
    mkdir "${FILEPATH}/RCS"
    ci -u ${FILE}
    co -l ${FILE}
else
    co -l ${FILE}
fi

if [ $? -lt 1 ]; then
    ${EDITOR} ${FILE}
    ci -u ${FILE}
fi
