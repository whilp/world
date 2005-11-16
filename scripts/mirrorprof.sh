#!/bin/sh

ORIG=p6wtb2h2.default
NEW=xydnkuvc.remote
PROFDIR=/home/will/.mozilla/firefox

# Get directory structure
cd ${ORIG}
for i in $(find . -type f); do
    cd ${PROFDIR}
    ln ${ORIG}/${i} ${NEW}/${i}
    echo "Linking ${ORIG}/${i} to ${NEW}/${i}"
done
