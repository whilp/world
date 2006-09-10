#!/bin/sh

BEGIN=$(date "+%Y.%m.%d %H:%M:%S")
TMPDIR=/tmp/tex
FILE=$1
FILENOX=${FILE%%.tex}
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

if [ "${FILE#*.}" != "tex" ]; then
    warn "File names must end in '.tex'."
    exit 1
fi

if [ ! -d "${TMPDIR}" ]; then
    notify 2 "Creating directory '${TMPDIR}'."
    mkdir -p "${TMPDIR}"
fi

cd $(dirname ${FILE})
cksum -a sha256 ${FILE} | sed -e 's/.* \(.*\)/\1/' >| "${FILE}.sha256-tmp"

if [ ! -f "${FILE}.sha256" ]; then
    notify 1 "Creating a sha256 hash for '${FILE}'."
    mv "${FILE}.sha256-tmp" "${FILE}.sha256"
else
    diff "${FILE}.sha256" "${FILE}.sha256-tmp" > /dev/null 2>&1
    if [ "$?" -gt 0 ]; then
        notify 1 "Beginning compilation of '${FILE}'."
        latex ${FILE} &&\
        bibtex ${FILENOX} &&\
        latex ${FILE} &&\
        latex ${FILE} &&\
        latex ${FILE} &&\

        notify 1 "Building PDF file." &&\
        pdflatex ${FILE} &&\

        notify 1 "Saving PDF ('${FILEPDF}') and its sha256 hash ('${FILEHASH}') in '${TMPDIR}'." &&\
        mv ${FILEPDF} ${TMPDIR} &&\
        cksum -a sha256 ${FILEEND} | sed -e 's/.* \(.*\)/\1/' >| ${FILEHASH}
    else
        notify 1 "File '${FILE}' appears to be unchanged."
    fi
fi
notify 1 "Build run for file '${FILE}' started at ${BEGIN} and finished at $(date "+%Y.%m.%d %H:%M:%S")."
