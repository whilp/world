#!/bin/sh

TODODIR=${HOME}/CVS/todo

cd ${TODODIR}
UPDATE="$(cvs -q up -Pd)"

if [ "${UPDATE}" ]; then
    DATE="$(date "+%Y.%m.%d %H:%M:%S")"
    MESSAGE="Automated CVS commit in ${TODODIR} on ${DATE}."
    MESSAGE="${MESSAGE}\n"
    MESSAGE="$(cvs commit -m "CRON: Automated commit")"
fi 

[ "${MESSAGE}" ] && echo "${MESSAGE}" | mail -s 'Automated CVS commit in ~/CVS/todo' will
