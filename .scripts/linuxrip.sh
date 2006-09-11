#!/bin/sh

ROOTDIR="/opt/storage/laura/Music"
PATTERN="%a/%l/%n - %t.ogg"

TMPDIR=$(mktemp -d $ROOTDIR/new.XXXXXX || exit 1)

cd "${TMPDIR}"

echo "Ripping CD in $ROOTDIR"
cdda2wav -paranoia -q -B -O wav -L 1 >/dev/null 2>&1

INDEXFILE=audio.cddb

TRACKS=$(sed -e '/^TTITLE/!d; s/ /__/g' $INDEXFILE)
AA=$(sed -e '/^DTITLE/!d; s/^DTITLE=//;' $INDEXFILE)
ARTIST="${AA%% / *}"
ALBUM="${AA##* / }"

echo "Encoding $ARTIST - $ALBUM"

for TRACK in ${TRACKS}; do
    TRACK=$(echo ${TRACK} | sed -e 's/__/ /g')
    NUM="${TRACK#TTITLE}"; NUM="${NUM%%=*}"
    NUM=$((NUM + 1))
    if [ "${NUM}" -lt 10 ]; then
        NUM="0${NUM}"
    fi
    TRACK="${TRACK#*=}"
    FILE="audio_${NUM}.wav"

    echo "Track $NUM: $TRACK"
    oggenc -Q -a "${ARTIST}" -t "${TRACK}" \
        -l "${ALBUM}" -N "${NUM}" \
        -q 9 -n "${PATTERN}" "${FILE}"
done

mv "${ARTIST}" ../
cd ../
rm -r "${TMPDIR}"
