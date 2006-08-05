#!/bin/sh

CMD="$1"

WEBROOT="/var/www"
WIKIROOT="${WEBROOT}/wiki"
WIKINAME="$2"
WIKIFCGI="${WIKIROOT}/${WIKINAME}/cgi-bin/moin.fcg"
WIKILOCK="${WIKIROOT}/.${WIKINAME}-lock"

if [ ! -f ${WIKIFCGI} ]; then
    echo "=!=> ${WIKIFCGI} doesn't exist!"
    exit 1
fi

case ${CMD} in
    start)
        if [ -f ${WIKILOCK} ]; then
            echo "=!=> Lockfile ${WIKILOCK} found!"
            exit 1
        fi
        echo "===> Starting ${WIKINAME}."
        touch ${WIKILOCK} && python ${WIKIFCGI} &
        ;;
    stop)
        if [ ! -f ${WIKILOCK} ]; then
            echo "=!=> No lockfile (${WIKILOCK}) found!"
            exit 1
        fi
        echo "===> Stopping ${WIKINAME}."
        rm ${WIKILOCK} && pkill -f "python.*${WIKIFCGI}"
        ;;
    *)
        echo "=!=> Command '${CMD}' unknown; must be either 'start' or 'stop.'"
        exit 1
        ;;
esac
