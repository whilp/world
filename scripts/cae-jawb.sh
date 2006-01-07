#!/bin/sh

USER_SSH=wcmaier
HOST_SSH=jawbreaker.cae.wisc.edu

# Start a master ssh connection in the background; remember to kill
# it later.
ssh -Nfn ${USER_SSH}@${HOST_SSH}

screen -S CAE-WORK -c $HOME/.screen/rc-cae-work

# I can't think of a better way to do this right now; shouldn't
# there be some way to trap the pid of the ssh -Nfn... process when
# it's created?  Oh well...
MASTERPID=$(pgrep -lf "ssh -Nfn ${USER_SSH}@${HOST_SSH}" |\
    head -1 |\
    sed -e 's/ .*//')

# Kill the master ssh session; this should kill leftover connections
# to the server as well.
kill ${MASTERPID}
