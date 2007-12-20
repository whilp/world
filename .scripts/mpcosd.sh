#!/bin/sh

mpc | head -2 | osd_cat \
    -p top \
    -o 25 \
    -A right \
    -i 3 \
    -f '-xos4-terminus-bold-*-*-*-24-*-*-*-*-*-*-*' \
    -c red
