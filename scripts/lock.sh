#!/bin/sh -x

ENVIRONMENT=${HOME}/.environment
LOCK_VOL=''
if [ $1 -eq "pause" ]; then
    PAUSE=1
else
    PAUSE=
fi


if [ -x "$(which tunes)" -a "$(grep '^PREF_LOCK_VOL=1' ${ENVIRONMENT})" ]; then
    LOCK_VOL=1
fi

if [ "${PAUSE}" ]; then
    [ "${LOCK_VOL}" ] && tunes slide && tunes toggle &
    xlock -mode blank
    [ "${LOCK_VOL}" ] && tunes toggle
    [ "${LOCK_VOL}" ] && tunes slide &
else
    [ "${LOCK_VOL}" ] && tunes slide &
    xlock -mode blank
    [ "${LOCK_VOL}" ] && tunes slide &
fi
