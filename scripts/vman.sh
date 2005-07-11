#!/usr/bin/env sh

MAN='/usr/bin/man'
VIM=`which vim`

echo $VIM

if [ $VIM ]; then
    man $* | col -bp | iconv -c | view -c 'set ft=man nomod nolist nofoldenable' -
else
    man $*
fi
