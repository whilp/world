#!/bin/sh

CVSDIR=$HOME/CVS

FIND=/usr/bin/find
CVS=/usr/bin/cvs

for dir in $(${FIND} ${CVSDIR} -type d); do
    cd ${dir} && ${CVS} up
done
