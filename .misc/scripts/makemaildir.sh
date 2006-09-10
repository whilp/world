#!/usr/bin/env sh

DIR=`pwd`


mkdir -p $DIR/Maildir/cur
mkdir -p $DIR/Maildir/new
mkdir -p $DIR/Maildir/tmp

chmod -R 700  $DIR/Maildir
