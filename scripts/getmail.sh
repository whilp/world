#!/bin/sh

/usr/local/bin/getmail $* --rcfile getmailrc-cae \
	--rcfile getmailrc-fm  \
	--rcfile getmailrc-hep  \
	--rcfile getmailrc-uw  \
	--rcfile getmailrc-gmail
# /home/will/bin/mailcheck > /home/will/Maildir/.count
