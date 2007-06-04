#!/bin/sh

DIR="/home/will/music"

menu () {
    if [ -n "$1" ]; then
        dmenu -b -p $1
    else
        dmenu -b
    fi
}

lsmusic () {
    for ITEM in "$1"/*; do
        [ -d "${ITEM}" ] && echo "${ITEM##*/}/"
        [ -f "${ITEM}" ] && echo "${ITEM##*/}"
    done
}

choosefile () {
    CHOICE=$(lsmusic "${DIR}" | menu "${DIR}:")
    if [ -z "${CHOICE}" ]; then
        exit
    elif [ "${CHOICE}" = ".." ]; then
        DIR=$(dirname ${DIR})
        choosefile "${DIR}"
    elif [ -d "${DIR}/${CHOICE}" ]; then
        DIR="${DIR}/${CHOICE%%/*}"
        choosefile "${DIR}"
    else
        FILE="${DIR}/${CHOICE}"
        case "${FILE}" in
            *.radio)
                cat ${FILE}
                ;;
            *)
                echo ${FILE}
                ;;
        esac
    fi
}

FILE=$(choosefile)
echo loadfile $FILE >> ~/.mplayer/fifo
