#!/bin/sh

ENVIRONMENT=${HOME}/.environment
LOCK_VOL=''

if [ -x "$(which tunes)" -a "$(grep '^PREF_LOCK_VOL=1' ${ENVIRONMENT})" ]; then
    LOCK_VOL=1
fi

[ "${LOCK_VOL}" ] && tunes slide &
xlock -mode blank
[ "${LOCK_VOL}" ] && tunes slide &
