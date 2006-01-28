#!/bin/sh

ARCH=
ERRATA=$(curl -s http://www.openbsd.org/pkg-stable.html |\
    grep 'ftp://.*.tgz' |\
    cut -f 2 -d '"' |\
    while read ERRATUM; do basename $ERRATUM .tgz; done)

while [ "$#" -gt 0 ]; do
    case "x$1" in
	x-h|x--help)
	;;
	*)
	;;
    esac
    shift
done


for ERRATUM in $ERRATA; do
    # echo $ERRATUM
    STRING=$(echo $ERRATUM | sed -e 's/\(.*-\)\(.*\)/\1<\2/')
    # echo pkg_info -qe $STRING
    pkg_info -qe $STRING
    if [ $? -lt 1 ]; then
	OLD=$(pkg_info -eq $STRING)
	echo "$OLD -> $ERRATUM"
    fi
done
