#!/bin/sh

ROOTCVS=${HOME}/CVS
NOTESDIR=${ROOTCVS}/notes
DOTFILESDIR=${ROOTCVS}/dotfiles
SCHOOLDIR=${ROOTCVS}/school

UPDATEDIRS="${SCHOOLDIR} ${DOTFILESDIR} ${NOTESDIR}"

for DIR in ${UPDATEDIRS}; do
    echo "Updating ${DIR} at $(date)."
    cd ${DIR}
    cvs -q up |\
    while read LINE; do
	echo "  ${LINE}"
    done
    echo ""
done
