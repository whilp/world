#!/bin/sh

for i in $(cat manifest); do
    ln -s $PWD/$i $HOME/bin/$(basename $i .sh)
done
