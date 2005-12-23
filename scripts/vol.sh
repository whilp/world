#!/bin/sh
##################  BEGIN HEADERS
# Filename	: vol.sh
# Use		: adjust the volume on an OpenBSD (3.8+) machine
# Author	: Will Maier <willmaier@ml1.net>
# Version	: $Revision: 1.5 $
# Updated	: $Date: 2005/12/23 16:11:58 $
# Vim		: :vim: set ft=sh:
# CVS		: $Id: vol.sh,v 1.5 2005/12/23 16:11:58 will Exp $
# Copyright	: Copyright (c) 2005 Will Maier
# License	: Expat; see <http://www.opensource.org/licenses/mit-license.php>
##################  END HEADERS

# Die if mixerctl is already running
if [ -n "$(pgrep mixerctl)" ]; then
    return 1
fi

# Programs used
BC=/usr/bin/bc
CUT=/usr/bin/cut
GREP=/usr/bin/grep
MIXERCTL=/usr/bin/mixerctl
SED=/usr/bin/sed

# Influential variables
AMOUNT=0
CHANGE=0
CURVOL=$(${MIXERCTL} outputs.master |\
    ${SED} 's/.*=//' |\
    ${CUT} -d ',' -f 1)
INCREMENT=20
MAXVOL=255
MIXERCTLCONF=/etc/mixerctl.conf
MUTE=
NORMALVOL=$(${GREP} 'outputs.master=' ${MIXERCTLCONF} |\
    ${CUT} -d ',' -f 2)
VERBOSE=

changeVol () {
    # Apply changes to the volume
    ${MIXERCTL} -q outputs.master=${NEWVOL},${NEWVOL}
}

percentVol () {
    # Returns the percentage volume left of the decimal point. bc(1)
    # needs the '-l' flag to increase the precision of its results
    # (ie 7.342); sed(1) will eliminate anything after and including
    # the decimal point.
    ${BC} -l -e "100*($1/${MAXVOL})" -e quit |\
    ${SED} 's/\..*//'
}

# If we didn't get any arguments, simply report the current volume
if [ "$#" -eq 0 ]; then
    echo "Current master volume: $(percentVol ${CURVOL})%"
    exit
fi

# Loop through the positional arguments
while [ "$#" -gt 0 ]; do
    case "x$1" in 
	x-u|x--up)
	# Increment the amount to change the volume; this can be
	# requested several times. The sum of the changes will be
	# applied to the master volume.
	CHANGE=$((CHANGE + INCREMENT))
	shift
	;;
	x-d|x--down)
	# Decrease the amount to change the volume; see above.
	CHANGE=$((CHANGE - INCREMENT))
	shift
	;;
	x-i|x--increment)
	# Define the amount by which to increment the volume;
	# passing this to us later in the chain of arguments might
	# produce some interesting results.
	shift
	INCREMENT=$1
	shift
	;;
	x-n|x--normal)
	NORMAL=1
	shift
	;;
	x-m|--mute)
	# Fade sounds to a (near) mute
	MUTE=1
	shift
	;;
	x-t|--toggle)
	# Fade from low to high or vice versa
	if [ "${CURVOL}" -lt "$((NORMALVOL - 20))" ]; then
	    NORMAL=1
	else
	    MUTE=1
	fi
	shift
	;;
	x-m|x--mute)
	# Fade sounds all the way out
	MUTEOFF=1
	shift
	;;
	x-v|x--verbose)
	# Allow reporting of statuses and whatnot.
	VERBOSE=1
	shift
	;;
	*)
	# Return an error and suggest that the user read the code.
	echo "Error: argument '$1' not recognized."
	echo "Usage: $0 [-u] [-d] [-i] [-v]"
	return 1
	;;
    esac
done

# Determine the desired volume.
NEWVOL=$((CURVOL + CHANGE))

# If the user wants to hear it, tell them what we're doing.
if [ ${CHANGE} -gt 0 -a "${VERBOSE}" ]; then
    echo "Raising master volume to $(percentVol ${NEWVOL})%"
elif [ ${CHANGE} -lt 0 -a "${VERBOSE}" ]; then
    echo "Lowering master volume to $(percentVol ${NEWVOL})%"
elif [ "${MUTE}" ]; then
    # Do a fancy fade out and quit
    while [ "${NEWVOL}" -gt "50" ]; do
	AMOUNT=$((AMOUNT + 10))
	NEWVOL=$((CURVOL - AMOUNT))
	changeVol
	sleep .1
    done
    exit
elif [ "${NORMAL}" ]; then
    # Do a fancy fade in and quit
    while [ "${NEWVOL}" -lt "${NORMALVOL}" ]; do
	AMOUNT=$((AMOUNT + 10))
	NEWVOL=$((CURVOL + AMOUNT))
	changeVol
	sleep .1
    done
    exit
elif [ ${CHANGE} -eq 0 -a "${VERBOSE}" ]; then
    echo "Maintaining volume at $(percentVol ${NEWVOL})%"
    exit
fi

# Finally, apply the new volume. mixerctl(1) wants the '-q' flag to
# not output a bunch of junk when it changes the variable.
changeVol
