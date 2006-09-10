#!/usr/bin/env sh
##################  BEGIN HEADERS
# Filename	: $HOME/bin/bsdbatt.sh
# Use		: displays the percentage charge remaining in the system
#		  battery
# Author	: Will Maier <willmaier@ml1.net>
# Started	: 2005.08.26
# Referred to by: $HOME/.screenrc
# Updated	: 2005.09.12 08:52:47 -0500
# Copyright	: Copyright (c) 2005 Will Maier
# License	: Expat; see <http://www.opensource.org/licenses/mit-license.php>
##################  END HEADERS

GREP=/usr/bin/grep
SYSCTL=/sbin/sysctl
AWK=/usr/bin/awk

PERCENT="$($SYSCTL -a | $GREP -i 'battery\.life' | $AWK '{print $2}')"
if [ "$PERCENT" -lt "98" ]; then
    echo $PERCENT
fi
