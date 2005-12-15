#!/bin/sh

for i in $(cat manifest); do
    ln -s $CWD/$i $HOME/bin/$(basename $i .sh)
done
