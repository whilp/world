#!/bin/sh

DSH="/usr/local/bin/dsh"
SSH="$(which ssh)"
SCP="$(which scp)"
DISPLAY=''
CONFIG="~/.dsh/config"
USER="will"

CLUSTER=${CLUSTER:-${CONFIG}}
FANOUT=8
RCMD_CMD=${RCMD_CMD:-${SSH}}
RCP_CMD=${RCP_CMD:-${SCP}}
RSHPORT=22
RCMD_USER=${RCMD_USER:-${WILL}}

# HEP-specific
if [ ${0##*/} = 'hsh' ]; then
    CLUSTER="~/.dsh/config-hep"
    RCMD_USER="wcmaier"
fi

dsh -f ${FANOUT} -t -o 2 -p 22 $*
#dsh $*
