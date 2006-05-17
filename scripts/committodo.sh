#!/bin/sh

TODODIR=${HOME}/CVS/todo

cd ${TODODIR}
cvs up >/dev/null 2>&1

if [ $? ]; then
    DATE="$(date "+%Y.%m.%d %H:%M:%S")"
    echo "Automated CVS commit in ${TODODIR} on ${DATE}."
    cvs commit -m "CRON: Automated commit"
fi
