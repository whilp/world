#!/bin/sh

DIR=~/music
CMDFIFO=~/.mplayer/fifo
STATIONS="
wefunk|http://www.wefunkradio.com/play/shoutcast.pls
slayradio|http://sc.slayradio.org:8000/listen.pls"

menu () {
    if [ -n "$1" ]; then
        dmenu -b -p "$1"
    else
        dmenu -b
    fi
}

play () {
    echo "loadfile $1" >> "${CMDFIFO}"
}

lsmusic () {
    for ITEM in "$1"/*
    do
            [ -d "${ITEM}" ] && echo "${ITEM##*/}/"
    done
    for ITEM in "$1"/*
    do
            [ -f "${ITEM}" ] && echo "${ITEM##*/}"
    done
}

chooseradio () {
    CHOICE=$(for S in $STATIONS; do echo "${S%%?http*}"; done | menu "Radio Stations:")
    #CHOICE=$(printf "wefunk\nslayradio\n" | menu "Radio Stations:")
    case "x${CHOICE}" in
        xwefunk)
            URL="http://www.wefunkradio.com/play/shoutcast.pls" ;;
        xslayradio)
            URL="http://sc.slayradio.org:8000/listen.pls" ;;
    esac
    echo "${URL}"
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
        echo "${DIR}/${CHOICE}"
    fi
}

choose () {
    cat <<EOF | menu "Categories:"
files
radio
EOF
}

CHOICE=$(choose | tr '[A-Z]' '[a-z]')
case "x${CHOICE}" in
    xfiles)
        FILE=$(choosefile)
        if [ -f "${FILE}" ]; then
            play "${FILE}"
        fi
        ;;
    xradio)
        STREAM=$(chooseradio)
        play "${STREAM}"
        ;;
    x)
        exit
        ;;
esac
