#!/bin/sh

for i in $(cat manifest); do
    chmod u+x $i
    ln -s $PWD/$i $HOME/bin/$(basename $i .sh)
done
