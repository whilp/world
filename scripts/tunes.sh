#!/bin/sh

MPD_HOST=messenger
MPC_COMMAND=
INCREMENT=7
CURVOL=$(MPD_HOST=${MPD_HOST} mpc volume | sed -e 's/[^0-9]//g')
MINVOL=30
MAXVOL=80

if [ $# -eq 0 ]; then
    # Just open ncmpc in a new window
    term -e "ncmpc --host ${MPD_HOST}"
else
    case $1 in
        toggle)
            MPC_COMMAND=toggle
            ;;
        volume)
            MPC_COMMAND="volume $2"
            ;;
        up|down)
            # just ignore -- they're special
            ;;
        *)
            echo "Bad command: $1."
            ;;
    esac
    MPD_HOST=${MPD_HOST} mpc ${MPC_COMMAND}

    # Special tricky slides
    case $1 in
        down)
            while [ ${CURVOL} -gt ${MINVOL} ]; do
                MPD_HOST=${MPD_HOST} mpc volume -${INCREMENT}
                CURVOL=$(MPD_HOST=${MPD_HOST} mpc volume | sed -e 's/[^0-9]//g')
            done
            ;;
        up)
            while [ ${CURVOL} -lt ${MAXVOL} ]; do
                MPD_HOST=${MPD_HOST} mpc volume +${INCREMENT}
                CURVOL=$(MPD_HOST=${MPD_HOST} mpc volume | sed -e 's/[^0-9]//g')
            done
            ;;
    esac
fi

