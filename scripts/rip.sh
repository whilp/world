#!/bin/sh

MUSIC=$HOME/Music
DIR=

LINENR=0
echo 'cddb' | cdio 2>/dev/null | while read LINE; do
    LINENR=$((LINENR + 1))
    if [ ${LINENR} -eq 1 ]; then
        export ARTIST=${LINE%% / *}
        export ALBUM=${LINE##* / }
        export ALBUM=${ALBUM%%\(*\)}
        export DIR="${MUSIC}/${ARTIST}/${ALBUM}"
        echo "===> Beginning rip in ${DIR}."
        mkdir -p "${DIR}"
        cd "${DIR}"
    elif [ ${LINENR} -gt 3 ]; then
        TRACKNR=$(echo ${LINE} | cut -d ' ' -f 1)
        if [ ${TRACKNR} -lt 10 ]; then
            TRACKNR="0${TRACKNR}"
        fi
        LENGTH=$(echo ${LINE} | cut -d ' ' -f 2)
        TITLE=$(echo ${LINE} | cut -d ' ' -f 3-)

        FILENAME="${DIR}/${TRACKNR} - ${TITLE}.wav"
        echo "===> Ripping track ${TRACKNR}: ${TITLE}."
        # TODO: It would be nice to:
        # a) encode only if we know cdparanoia successfully ripped a
        # track.
        # b) monitor the children (ie wait until they've died to
        # actually quit.
        # This is complicated by the fact that this entire while
        # loop runs in a subshell (due to the pipe), and so wait(1)
        # doesn't see the subshell's children. Boo.
        cdparanoia -q ${TRACKNR} "${FILENAME}" &&\
        flac --silent --best -T ARTIST="${ARTIST}" \
            -T NUMBER="${TRACKNR}" \
            -T ALBUM="${ALBUM}" \
            -T TITLE="${TITLE}" "${FILENAME}"
    fi
    LINENR=$((LINENR + 1))
done
wait
exit

# cd "${DIR}"
# ARTIST=${PWD%/*}; ARTIST=${ARTIST##*/}
# ALBUM=${PWD##*/}
# echo "===> Beginning FLAC encode for ${ARTIST} - ${ALBUM}."
# flac --silent --best *
# for FILE in *.flac; do
#     NUMBER=${FILE%% - *}
#     TITLE=${FILE##* - }; TITLE=${TITLE%.flac}
# 
#     metaflac --set-tag=ARTIST="${ARTIST}" \
#             --set-tag=NUMBER="${NUMBER}" \
#             --set-tag=ALBUM="${ALBUM}" \
#             --set-tag=TITLE="${TITLE}" "${FILE}"
# done
