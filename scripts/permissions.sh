#!/usr/bin/env sh

CVS=$HOME/CVS
MODULE=dotfiles
MODPATH=$CVS/$MODULE

find $MODPATH -type f | xargs chmod 600
find $MODPATH/scripts -type f | xargs chmod 700
