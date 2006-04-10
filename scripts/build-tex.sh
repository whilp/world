#!/bin/sh

TMPDIR=/tmp/tex
FILE=$1
FILENOX=${1%%.tex}
FILEPDF="${FILENOX}.pdf"
FILEEND="${TMPDIR}/${FILEPDF}"
FILEHASH="${FILEEND}.sha256"
VERBOSE=1

notify () {
    if [ "$#" -eq 1 ]; then
        LEVEL=1
        MESSAGE=$1
    else
        LEVEL=$1
        MESSAGE=$2
    fi 

    if [ "${LEVEL}" -ge "${VERBOSE}" ]; then
        echo "===> ${MESSAGE}"
    fi
}
warn () {
    MESSAGE=$1
    echo "=!=> ${MESSAGE}"
}


if [ ! -d "${TMPDIR}" ]; then
    notify 2 "Creating directory '${TMPDIR}'."
    mkdir -p "${TMPDIR}"
fi

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
