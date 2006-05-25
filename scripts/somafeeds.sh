#!/bin/sh

SITE=http://somafm.com

STREAMS="$(curl -s "${SITE}" | sed -e '/\.pls/!d; s/^.*href="\///; s/">.*$//; /[0-9]\{2\}\.pls$/d')"

GOODSTREAMS=''
URL=''
for STREAM in ${STREAMS}; do
    TARGET="${SITE}/${STREAM}"
    URL=$(curl -s "${TARGET}" | sed -e '/File1/!d; s/.*=//' || exit 1)
    curl -svi "${URL}" 2>/dev/null | head -10 | grep '^icy-url:.*somafm.com' >/dev/null 2>&1
    if [ "$?" ]; then
        case STREAM in
            ${GOODSTREAMS})
                # Ignore it
            ;;
            *)
                GOODSTREAMS="${GOODSTREAMS} ${STREAM}"
                echo "${URL}"
            ;;
        esac
    fi
done | sort -u
