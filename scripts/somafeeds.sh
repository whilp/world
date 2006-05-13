#!/bin/sh

SITE=http://somafm.com

STREAMS="$(curl -s "${SITE}" | sed -e '/\.pls/!d; s/^.*href="\///; s/">.*$//; /[0-9]\{2\}\.pls$/d')"

for STREAM in ${STREAMS}; do
    curl -s "${SITE}/${STREAM}" | sed -e '/File1/!d; s/.*=//'
done
