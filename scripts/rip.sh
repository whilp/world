#!/bin/sh

MUSIC=$HOME/Music

LINENR=0
echo 'cddb' | cdio 2>/dev/null | while read LINE; do
    LINENR=$((LINENR + 1))
    if [ ${LINENR} -eq 1 ]; then
        # Ellen Allien / Berlinette(newage)
        export ARTIST=${LINE%% / *}
        export ALBUM=${LINE##* / }
        export ALBUM=${ALBUM%%\(*\)}
        export DIR="${MUSIC}/${ARTIST}/${ALBUM}"
        echo "===> Beginning rip in ${DIR}."
        mkdir -p "${DIR}"
    elif [ ${LINENR} -gt 3 ]; then
        #     1   3:54.24  Alles Sehen
        TRACKNR=$(echo ${LINE} | cut -d ' ' -f 1)
        if [ ${TRACKNR} -lt 10 ]; then
            TRACKNR="0${TRACKNR}"
        fi
        LENGTH=$(echo ${LINE} | cut -d ' ' -f 2)
        TITLE=$(echo ${LINE} | cut -d ' ' -f 3-)

        FILENAME="${DIR}/${TRACKNR} - ${TITLE}.wav"
        echo "===> Ripping track ${TRACKNR}: ${TITLE}."
        cdparanoia -q ${TRACKNR} "${FILENAME}"
    fi
    LINENR=$((LINENR + 1))
done

echo "===> Beginning FLAC encode for ${ARTIST} - ${ALBUM}."
cd ${DIR}
flac --silent --best *
