#!/bin/sh
# Set FLAC metadata

# Arrays and things work with OpenBSD pdksh/sh

# Options
MANIFEST="${PWD}/manifest"
VERBOSE="3"
ECHO=echo
ME=$(basename "$0")

# Functions
notify () {
    if [ "$#" -lt 2 ]; then
        LEVEL=1
        MESSAGE="$1"
    else
        LEVEL="$1"
        MESSAGE="$2"
    fi
    if [ "${LEVEL}" -ge "${VERBOSE}" ]; then
        echo "===> ${MESSAGE}"
    fi
}
warn () {
    echo "=!=> $1"
}
die () {
    if [ "$#" -lt 2 ]; then
        EXIT=1
        MESSAGE="$1"
    else
        EXIT="$1"
        MESSAGE="$2"
    fi
    warn "${MESSAGE}"
    exit "${EXIT}"
}
usage () {
    echo "USAGE"
}

# Parse args
notify 3 "Parsing command line options."
while getopts "em:t:n:" ARG; do
    case "${ARG}" in
        e)
            ECHO="echo"
            notify 3 "'ECHO' set to '${ECHO}'."
            ;;
        m) 
            MANIFEST=${OPTARG}
            notify 3 "'MANIFEST' set to '${MANIFEST}'."
            ;;
        t)
            TYPE=${OPTARG}
            notify 3 "'TYPE' set to '${TYPE}'."
            ;;
        n)
            NUMGLOB=${OPTARG}
            notify 3 "'NUMGLOB' set to '${NUMGLOB}'."
            ;;
    esac
done

# If we don't have any of this important stuff, die
if [ ! -r "${MANIFEST}" ]; then
    die "Can't read manifest file '${MANIFEST}.'"
elif [ ! "${TYPE}" ]; then
    die "You must specify a media type on the command line ('${ME} -t TYPE')." 
    usage
elif [ ! "${NUMGLOB}" ]; then
    warn "You must either explicitly accept the default file parsing pattern"
    warn "('${ME} -n default'; works with files like track01.cdda.wav) or specify"
    warn "a different pattern using the '-n' flag."
    usage
    exit 1
fi

case "${TYPE}" in
    mp3) 
        EXTENSION=mp3
        PROGRAM=id3ed
        ;;
    ogg) 
        EXTENSION=ogg
        PROGRAM=vorbiscomment
        ;;
    flac) 
        EXTENSION=flac
        PROGRAM=metaflac
        ;;
    *)
        die "Media type '${TYPE}' not recognized."
    ;;
esac
notify 3 "Using '${PROGRAM}' to edit ${TYPE} metadata."

if [ ! -x $(which ${PROGRAM}) ]; then
    die "Can't find metadata editing program '${PROGRAM}.'"
fi

# Get album information from the manifest
notify 3 "Extracting metadata from manifest file '${MANIFEST}'."
. "${MANIFEST}"

# Clear existing metadata (if supported)
case ${PROGRAM} in
    id3ed)
        # Does this work?
        REMOVEARGS="-r"
        ;;
    vorbiscomment)
        # Does not support clearing tags
        REMOVEARGS=
        ;;
    metaflac)
        REMOVEARGS="--remove-all"
esac
if [ "${REMOVEARGS}" ]; then
    ${ECHO} "${PROGRAM}" "${REMOVEARGS}" *${EXTENSION}
    notify 2 "Clearing ${TYPE} metadata."
else
    notify 1 "'${PROGRAM}' can't clear metadata; current data will be simply overwritten."
fi

notify 1 "Editing metadata for ${TYPE} files in ${PWD}."
notify 2 "\tArtist: ${ARTIST}"
notify 2 "\tAlbum: ${ALBUM}"
notify 2 "\tGenre: ${GENRE}"
notify 2 "\tDate: ${DATE}"

for FILE in $PWD/*.${EXTENSION}; do
    echo HERE $FILE
    notify 3 "Processing '$(basename ${FILE}).'"
    # This works, assuming you use cdparanoia's default naming
    # scheme: track##.cdda.wav
    notify 1 "Editing file '$(basename "$FILE")'"
    if [ "${NUMGLOB}" = 'default' ]; then
        NUMGLOB='s/[^0-9]//g'
        # NUMGLOB='s/\(..\).*/\1/g'
    fi
    NUMBER=$(echo $(basename "$FILE") | sed -e "${NUMGLOB}")
    echo HERE $NUMBER
    # Remove leading zeros, if any; this causes pdksh to whine about
    # 'bad numbers' when trying to access elements in the array.
    # Harrumph. This isn't a problem for, eg 0[1-7], but errors on
    # eg 08.
    notify 2 "\tTrack: ${NUMBER}\tTitle: ${TITLE}"
    case "${PROGRAM}" in
        metaflac)
            "${ECHO}" "${PROGRAM}" --set-tag=ALBUM="${ALBUM}" \
                --set-tag=ARTIST="${ARTIST}" \
                --set-tag=GENRE="${GENRE}" \
                --set-tag=DATE="${DATE}" \
                --set-tag=TRACKNUMBER="${NUMBER}" \
                --set-tag=TITLE="${TITLE}" "${FILE}"
            ;;
        id3ed)
            "${ECHO}" "${PROGRAM}" -a "${ALBUM}" \
                -n "${ARTIST}" \
                -g "${GENRE}" \
                -y "${DATE}" \
                -k "${NUMBER}" \
                -s "${TITLE}" "${FILE}"
            ;;
        vorbiscomment)
            "${ECHO}" "${PROGRAM}" -t "ALBUM='${ALBUM}'" \
                -t "ARTIST='${ARTIST}'" \
                -t "GENRE='${GENRE}'" \
                -t "DATE='${DATE}'" \
                -t "TRACKNUMBER='${NUMBER}'" \
                -t "TITLE='${TITLE}'" "${FILE}"
            ;;
    esac
    # Clear out all but a-zA-Z0-9; it'd be nice to let [.,] and
    # other characters that aren't interpreted by the shell. Oh
    # well.
    sanitize () {
        INPUT="$1"
        OUTPUT=$(echo "$INPUT" | tr -c "[:alnum:]" "_")
        echo "${OUTPUT}"
    }

    ALBUM=$(sanitize "${ALBUM}")
    ARTIST=$(sanitize "${ARTIST}")
    TITLE=$(sanitize "${TITLE}")

    if [ "${NUMBER}" -lt 10 ]; then
        NUMBER="0${NUMBER}"
    fi

    # Rename files to match the naming scheme.
    NEWFILE="${NUMBER}-[${ALBUM}]-${TITLE}.${EXTENSION}"
    "${ECHO}" mv "${FILE}" "${NEWFILE}"
done

notify 1 "Metadata editing complete."

# Data should look like this:
# ARTIST="Wu-Tang Clan"
# ALBUM="Wu-Tang Forever (Disc 2)"
# GENRE="Hip Hop"
# DATE="1997"
# 
# TRACK[1]="Intro"
# TRACK[2]="Triumph (feat. CappaDonna)"
# TRACK[3]="Impossible (feat. Tekitha)"
# TRACK[4]="Little Ghetto Boys (feat. CappaDonna)"
# TRACK[5]="Deadly Melody (feat. Street Life)"
# TRACK[6]="The City"
# TRACK[7]="The Projects"
# TRACK[8]="Bells of War"
# TRACK[9]="The M.G.M."
# TRACK[10]="Dog Shit"
# TRACK[11]="Duck Seazon"
# TRACK[12]="Hellz Wind Staff (feat. Street Life)"
# TRACK[13]="Heaterz (feat. CappaDonna)"
# TRACK[14]="Black Shampoo"
# TRACK[15]="Second Coming (feat. Tekitha)"
# TRACK[16]="The Closing"
