#!/bin/sh
##################  BEGIN HEADERS
# Filename	: vol.sh
# Use		: adjust the volume on an OpenBSD (3.8+) machine
# Author	: Will Maier <willmaier@ml1.net>
# Version	: $Revision: 1.1 $
# Updated	: $Date: 2005/12/14 21:39:13 $
# Vim		: :vim: set ft=sh:
# CVS		: $Id: vol.sh,v 1.1 2005/12/14 21:39:13 will Exp $
# Copyright	: Copyright (c) 2005 Will Maier
# License	: Expat; see <http://www.opensource.org/licenses/mit-license.php>
##################  END HEADERS

# Programs used
BC=/usr/bin/bc
CUT=/usr/bin/cut
MIXERCTL=/usr/bin/mixerctl
SED=/usr/bin/sed

# Influential variables
CHANGE=0
CURVOL=$(${MIXERCTL} outputs.master | ${SED} 's/.*=//' | ${CUT} -d ',' -f 1)
INCREMENT=20
MAXVOL=255
VERBOSE=

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
	x-d|--down)
	# Decrease the amount to change the volume; see above.
	CHANGE=$((CHANGE - INCREMENT))
	shift
	;;
	x-i|--increment)
	# Define the amount by which to increment the volume;
	# passing this to us later in the chain of arguments might
	# produce some interesting results.
	shift
	INCREMENT=$1
	echo $INCREMENT
	shift
	;;
	x-v|--verbose)
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
elif [ ${CHANGE} -eq 0 -a "${VERBOSE}" ]; then
    echo "Maintaining volume at $(percentVol ${NEWVOL})%"
    exit
fi

# Finally, apply the new volume. mixerctl(1) wants the '-q' flag to
# not output a bunch of junk when it changes the variable.
${MIXERCTL} -q outputs.master=${NEWVOL},${NEWVOL}
