#!/bin/sh

# Per http://www.openbsd.org/checklist.html

PORT_NAME=
PORT_SECTION=
ME=${0##./}

usage () {
    cat <<- EOF
    Usage: ${ME} [-n name] [-s section]
    EOF
}
warn () {
    echo "=!=> $*"
}
note () {
    echo "=-=> $*"
}

while [ "$#" -eq 0 ]; then
    case "x$1" in
	x-n|x--name)
	shift
	PORT_NAME=$1
	;;
	x-s|x--section)
	shift
	PORT_SECTION=$1
	;;
	x-h|--help)
	usage
	;;
	*)
	shift
	;;
    esac
done

if [ ! ${PORT_NAME} -o ! ${PORT_SECTION} ]; then

    

/usr/ports/infrastructure/templates/Makefile.template
