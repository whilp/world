#!/usr/bin/env sh

if [ $1 ]; then
    FLAGS=$1
fi

/usr/local/bin/getmail $FLAGS \
	--rcfile getmailrc-cae \
	--rcfile getmailrc-fm \
	--rcfile getmailrc-gmail
