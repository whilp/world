#!/bin/sh

BEGIN=$(date "+%Y.%m.%d %H:%M:%S")
BUILDHOST=merk
TMPDIR=/tmp/tex
FILE=$1
FILENOX=${1%%.pdf}
FILEPDF="${FILENOX}.pdf"
FILEEND="${TMPDIR}/${FILEPDF}"
FILEHASH="${FILEEND}.sha256"
VERBOSE=${VERBOSE:-1}

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

if [ "${FILE#*.}" != "pdf" ]; then
    warn "File names must end in '.pdf'."
    exit 1
fi

if [ ! -d "${TMPDIR}" ]; then
    notify 2 "Creating directory '${TMPDIR}'."
    mkdir -p "${TMPDIR}"
fi

ssh ${BUILDHOST} "ls ${FILEEND} > /dev/null 2>&1 && ls ${FILEHASH} > /dev/null 2>&1 || return 1"
if [ "$?" -gt 0 ]; then
    warn "Couldn't find '${FILEEND}' or '${FILEHASH}' on build host ${BUILDHOST}."
    exit 1
fi

notify 1 "Copying '${FILEEND}' from ${BUILDHOST}."
scp ${BUILDHOST}:"${FILEEND}" "${FILEEND}-tmp" > /dev/null 2>&1
notify 1 "Copying '${FILEHASH}' from ${BUILDHOST}."
scp ${BUILDHOST}:"${FILEHASH}" "${FILEHASH}-tmp" > /dev/null 2>&1

if [ ! -f "${FILEEND}" ]; then
    notify 1 "No local PDF found; updating local copy."
    mv "${FILEHASH}-tmp" "${FILEHASH}"
    mv "${FILEEND}-tmp" "${FILEEND}"
    exit
elif [ ! -f "${FILEHASH}" ]; then
    notify 1 "Hashing local PDF."
    sha256deep | cut -d ' ' -f 1 >| "${FILEHASH}"
fi

notify 1 "Comparing hash files."
diff "${FILEHASH}" "${FILEHASH}-tmp" > /dev/null 2>&1
if [ "$?" -eq 1 ]; then
    notify 1 "Detected difference in the hash files; updating the local copy of '${FILEEND}'."
    mv "${FILEEND}-tmp" "${FILEEND}"
    mv "${FILEHASH}-tmp" "${FILEHASH}"
else
    notify 1 "Hash files are identical; removing temporary files."
    rm ${TMPDIR}/*-tmp
fi
notify 1 "Sync run for '${FILE}' started at ${BEGIN} and finished at $(date "+%Y.%m.%d %H:%M:%S")."
