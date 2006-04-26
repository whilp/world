#!/bin/sh

# TODO:
# Clean up divx2blah.log
# Estimate disk space required and encoding time

# Subroutines
notify () {
    if [ "$#" -eq 1 ]; then
        LEVEL=1
        MESSAGE=$1
    else
        LEVEL=$1
        MESSAGE=$2
    fi

    if [ "${LEVEL}" -le "${VERBOSE}" ]; then
        echo "===> ${MESSAGE}"
    fi
}
warn () {
    MESSAGE=$1
    echo "=!=> ${MESSAGE}"
}
encode () {
    SOURCE=${1:-dvd://1}
    PASS=${2:-2}
    VBITRATE=${3:-3}
    FILENAME=${4}

    mencoder ${SOURCE} -quiet \
        -ovc lavc \
        -lavcopts vcodec=mpeg4:vpass=${PASS}:vbitrate=${VBITRATE}:ilme:ildct \
        -oac copy \
        -o ${FILENAME}
}


# Parse args
if [ $# -eq 1 ]; then
    # Assume we're copying from the DVD drive; $TITLE should be the
    # first positional argument.
    TITLE=$1
elif [ $# -gt 0 ]; then
    while [ $# -gt 0 ]; do
        case "X$1" in
            X-t)
                shift
                TITLE=${1}
                ;;
            X-n)
                shift
                TITLENR=${1}
                ;;
            *)
                warn "Bad option: $1"
                exit 1
                ;;
        esac
        shift
    done
elif [ $# -eq 0 ]; then
    warn "Must specify a film title or other options on the command line."
    exit 1
fi

TITLENR=${TITLENR:-1}
BACKUPDIR="/opt/storage/DVD"
FILE="$(echo ${TITLE} | tr -s '[:upper:] ' '[:lower:]_')"
EXT=".avi"
FILENAME="${BACKUPDIR}/${FILE}${EXT}"
VBITRATE=5000
SOURCE="dvd://${TITLENR}"
PASSES=2
PASS=1

notify "Beginning encoding session for '${TITLE}'."
while [ ${PASS} -lt ${PASSES} ]; do
    LOG="${BACKUPDIR}/$(date "+%Y%m%d-%H%M")-${FILE}-${PASS}.log"
    notify "Session begun at $(date "+%Y.%m.%d %H:%M:%S")." >| ${LOG}
    notify "FILENAME: ${FILENAME}" >> ${LOG}
    notify "SOURCE: ${SOURCE}" >> ${LOG}
    notify "VIDEO BITRATE: ${VBITRATE}" >> ${LOG}
    notify "PASS: ${PASS}" >> ${LOG}
    echo "" >> ${LOG}
    notify "Encode command: TODO" >> ${LOG}

    notify "Logs for pass ${PASS} of this encode are stored in '${LOG}'."
    notify "Encode of '${TITLE}' started at $(date "+%Y.%m.%d %H:%M:%S")."

    time encode ${SOURCE} ${PASS} ${VBITRATE} ${FILENAME} >> ${LOG} 2>&1
    notify "Pass ${PASS} complete at $(date "+%Y.%m.%d %H:%M:%S)."
    PASS=$((PASS + 1))
done


# time $(mencoder dvd://1 -quiet -ovc lavc -lavcopts vcodec=mpeg4:vpass=1:vbitrate=5000 -oac copy -o ${FILENAME} && \
# mencoder dvd://1 -quiet -ovc lavc -lavcopts vcodec=mpeg4:vpass=2:vbitrate=5000 -oac copy -o ${FILENAME} > /dev/null)

# Stats:
#   Blue Velvet DVD:
#   mencoder dvd://1 -ovc lavc -lavcopts vcodec=mpeg4:vpass=1:vbitrate=5000:ilme:ildct -oac copy -o ${FILE} && \
#   mencoder dvd://1 -ovc lavc -lavcopts vcodec=mpeg4:vpass=2:vbitrate=5000:ilme:ildct -oac copy -o ${FILE}
#   1.6G	blue-velvet.avi

# vim: set tw=0:
