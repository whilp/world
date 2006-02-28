#!/bin/sh


WWWBASE='http://thislife.org/'
SHOWNAME="This American Life"
SHOWS="307 306 304 302 299"
#http://thislife.org/ra/309.ram

for SHOW in ${SHOWS}; do
    FIFO=$(mktemp ${PWD}/ayw-tal-fifo.XXXXXX)
    rm -rf ${FIFO}
    # SHOW-SHOWNR.EXT
    TYPE=ogg
    OFILE="tal-${SHOW}.${TYPE}"
    URL="${WWWBASE}/ra/${SHOW}.ram"
    REALURL=$(curl -s ${URL})
    # echo "SHOW: ${SHOW}"
    # echo "FIFO: ${FIFO}"
    # echo "TYPE: ${TYPE}"
    # echo "OFILE: ${OFILE}"
    # echo "URL: ${URL}"
    # echo "REALURL: ${REALURL}"
    mkfifo "${FIFO}"
    oggenc "${FIFO}" -o "${OFILE}" -t "Show Nr. ${SHOW}" \
        -a "${SHOWNAME}" &
    mplayer -really-quiet -cache 400 -ao pcm:file="${FIFO}" ${REALURL}
    rm -rf ${FIFO}
done
sleep 30
pkill -f "${FIFO}"
