#!/usr/bin/env sh

MAN='/usr/bin/man'
VIM=`which vim`

if [ $VIM ]; then
    $MAN $* | col -bp | iconv -c | $VIM -c 'set ft=man nomod nolist nofoldenable' -
else
    $MAN $*
fi
