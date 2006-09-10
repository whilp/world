#!/bin/sh

# TODO:
# -make $0 into a suite (eg ./package -a to add, -d to delete, etc)
#   -delete
#   -add/install [WCMAIER: DONE 20051122]
#   -info
#   -update
#	-compare output of 'info' to fresh INDEX file; note version
#	differences
# -add checks ('really want to install?') [WCMAIER: DONE 20051122]
# -add verbosity levels
# -add a usage message [WCMAIER: DONE  20051122]
# -add 'freshness' checks on INDEX file [WCMAIER: DONE 20051122]

GREP=/usr/bin/grep
PKGADD=/usr/sbin/pkg_add
SUDO=/usr/bin/sudo
WGET=/usr/local/bin/wget
WC=/usr/bin/wc
STAT=/usr/bin/stat
DATE=/bin/date
FTP=/usr/bin/ftp

ME=$(basename $0)

# Subroutines -- first by necessity
getPkgName () {
    ${GREP} $1 ${PKGINDEX}
}
downloadIndex () {
    ${SUDO} ${FTP} -o ${PKGINDEX} ${PKGPATH}/index.txt >/dev/null
}
usage () {
    echo "usage: ${ME} [-h] [-a <package>]"
    echo ""
    echo "  -h|--help		: Print this usage summary and exit."
    echo "  -a|--add <package>	: Install <package>; can specify multiple instances (ie -a <package1> -a <package2>)."
}

# Process command line arguments
if [ "$#" -eq 0 ]; then
    usage
    exit 1
fi
OPTADD=''
OPTDOWNLOAD=''
PACKAGERE=''	    # List of regular expressions to search on
while [ "$#" -gt 0 ]; do
    case "x$1" in
	x-a|x--add)
	OPTADD=1
	shift
	PACKAGERE="${PACKAGERE} $1"
	;;
	x-h|x--help)
	usage
	;;
	*)
	usage
	exit 1
	;;
    esac
    shift
done

PKGINDEX=/usr/packages/INDEX
PKGPATH=${PKG_PATH:-ftp://openbsd.mirrors.tds.net/pub/OpenBSD/3.8/packages/i386/}

# Check that PKGINDEX exists
if [ ! -r ${PKGINDEX} ]; then
    read REPLY?"====> Local copy of the packages index (${PKGINDEX}) not found; download new copy? [y/N] "
    case "x${REPLY}" in
	xy|xY|xyes|xYES)
	OPTDOWNLOAD=1
    esac
fi

SECONDSWEEK=$((60*60*24*7))
INDEXMTIME=$(${STAT} -L -f "%m" ${PKGINDEX})
TODAY=$(date "+%s")

if [ "$((TODAY - INDEXMTIME))" -gt "${SECONDSWEEK}" ]; then
    # This check works, but it makes sense to add a cron job to
    # update the file instead of relying on this program (or you) to
    # regularly update it.
    read REPLY?"====> Local copy of the packages index (${PKGINDEX}) over a week old; download new copy? [y/N] "
    case "x${REPLY}" in
	xy|xY|xyes|xYES)
	OPTDOWNLOAD=1
    esac
fi

if ["${OPTDOWNLOAD}" ]; then
    echo "====> Downloading new package index file to ${PKGINDEX}."
    downloadIndex
fi

if [ "${OPTADD}" ]; then
    RENR=$(echo ${PACKAGERE} | ${WC} -w)
    RENR=$(echo ${RENR})

    increment=1
    for index in ${PACKAGERE}; do
	echo "====> Processing regex ${increment} of ${RENR}: '${index}'."
# Find number of matching packages
	MATCHINGPKGNR=$(getPkgName ${index} | ${WC} -l)
	MATCHINGPKGNR=$(echo ${MATCHINGPKGNR})	    # Hack to remove leading
							# whitespace
# Generate a list of matching packages
	MATCHINGPKGS=$(getPkgName ${index})

	if [ "${MATCHINGPKGNR}" -gt "1" ]; then
	    # Found multiple matches; return the list and request a more
	    # specific regex
	    echo "====> Found ${MATCHINGPKGNR} matching packages; choose one and run '${ME}' again."
	    for i in ${MATCHINGPKGS}; do
		echo "    $i"
	    done
	    return 1
	elif [ "${MATCHINGPKGNR}" -lt "1" ]; then
	    echo "====> No matching packages found; consider changing the syntax of your regular expression."
	    exit 1
	else
	    # Install the package
	    read REPLY?"====> Install ${MATCHINGPKGS}? [y/N] "
	    case "x${REPLY}" in
		xy|xY|xyes|YES)
		echo "====> Installing ${MATCHINGPKGS}."
		${SUDO} ${PKGADD} ${PKGPATH}/${MATCHINGPKGS}
		;;
	    esac
	fi
	increment=$((increment +1 ))
    done
fi
