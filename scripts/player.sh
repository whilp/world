#!/usr/bin/env sh

CHECKTHIS=`ps aux | grep mpd | head -n 1 | mawk '{ print $11 '}`

if [ $CHECKTHIS == 'mpd' ]; then
    ncmpc
else
    mpd
    ncmpc
fi
