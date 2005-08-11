#!/usr/bin/env sh
##################  BEGIN HEADERS
# Filename	: other.sh
# Use		: starts program in a different x-server
# Author	: Will Maier <willmaier@ml1.net>
# Started	: 2005.08.09
# Based on	: Posts to ubuntuforums.org, thread 51486
# Updated	: 2005.08.09 16:29:37
##################  END HEADERS

xinit `which $*` -- :1 2> /dev/null&
