#!/bin/sh -x

ENVCACHE="${HOME}/.environment"
# MPD_CONF=/etc/mpd.conf
# MPD_STATE=$(grep state ${MPD_CONF} | sed -e 's/.*"\(\/.*\)"$/\1/')
MPD_PORT=6600
if [ -r "${ENVCACHE}" ]; then
    # If there's a file with cached environment variables on this
    # machine, look for relevant stuff and source it.
    PREF_MPD_HOST="$(sed -e '/^PREF_MPD_HOST=/!d; s/PREF_MPD_HOST=//' "${ENVCACHE}")"
fi

send () {
    echo $* | nc ${MPD_HOST} ${MPD_PORT}
}
curvol () {
    send status | grep '^volume:' | cut -d ':' -f 2 2>/dev/null
}

HOSTS="${MPD_HOST} ${PREF_MPD_HOST} localhost messenger"

for HOST in $HOSTS; do 
    # Work through the host list in order, testing connections to
    # the server for each. If we get something (_anything_) back on
    # stdin, set that host as MPD_HOST and break.
    if [ -n "$(MPD_HOST="${HOST}" send 2>/dev/null)" ]; then
        MPD_HOST=${HOST}
        break
    fi
done

# If we don't have anything to talk to, die now.
[ "${MPD_HOST}" ] || exit 1


[ -x "$(which mpc 2>/dev/null)" ] && \
    HAVE_MPC=1 || \
    HAVE_MPC=
MPC_COMMAND=
MPD_PORT=6600
INCREMENT=7
CURVOL="$(curvol)"
#CURVOL=$(MPD_HOST=${MPD_HOST} mpc volume | sed -e 's/[^0-9]//g')
MINVOL=30
MAXVOL=75
FORMAT="[[%artist% - ]%title%|%name%|%file%]"

if [ $# -eq 0 ]; then
    # Open ncmpc in a new window if it's not running; otherwise,
    # pause/resume playback.
    if [ ! "$(pgrep -lf "ncmpc --host ${MPD_HOST}")" ]; then
        term -e -T MPD "ncmpc --host ${MPD_HOST}"
    else
        send pause
        #MPD_HOST=${MPD_HOST} mpc toggle
    fi
else
    # Twiddle a knob.
    case $1 in
        stats|clear|add|next|stop)
            MPC_COMMAND="$1"
            ;;
        prev)
            MPC_COMMAND="previous"
            ;;
        toggle)
            MPC_COMMAND="pause"
            ;;
        volume)
            MPC_COMMAND="setvol $2"
            #MPC_COMMAND="volume $2"
            ;;
        up|down|query|slide|ncmpc)
            # just ignore -- they're special
            ;;
        *)
            echo "Bad command: $1."
            exit 1
            ;;
    esac
    [ "${MPC_COMMAND}" ] && send ${MPC_COMMAND} >/dev/null 2>&1 && exit
    #[ "${MPC_COMMAND}" ] && MPD_HOST=${MPD_HOST} mpc ${MPC_COMMAND} 2>/dev/null 2>&1 && exit

    case $1 in
        query)
            [ "${HAVE_MPC}" ] && MPD_HOST=${MPD_HOST} mpc --format "${FORMAT}"
            ;;
        ncmpc)
            term -T MPD -e "ncmpc --host ${MPD_HOST}"
            ;;
    esac

    # Special tricky slides
    case $1 in
        down)
            while [ ${CURVOL} -gt ${MINVOL} ]; do
                NEWVOL=$((CURVOL - INCREMENT))
                send setvol ${NEWVOL} >/dev/null 2>&1
                #MPD_HOST=${MPD_HOST} mpc volume -${INCREMENT} >/dev/null 2>&1
                CURVOL="$(curvol)"
                #CURVOL=$(send status | sed -e 's/[^0-9]//g')
            done
            ;;
        up)
            while [ ${CURVOL} -lt ${MAXVOL} ]; do
                NEWVOL=$((CURVOL + INCREMENT))
                send setvol ${NEWVOL} >/dev/null 2>&1
                #MPD_HOST=${MPD_HOST} mpc volume +${INCREMENT} >/dev/null 2>&1
                CURVOL="$(curvol)"
                #CURVOL=$(MPD_HOST=${MPD_HOST} mpc volume | sed -e 's/[^0-9]//g')
            done
            ;;
        slide)
            # The modulo bits are intended to smooth the slide;
            # there may be a better method, but it seems rather
            # efficient to do it this way.
            # TODO: This doesn't make sense; if $INC only
            # increments when [$((INC % 2)) -eq 0], the 
            INCINC=0
            if [ ${CURVOL} -gt 50 ]; then
                INC=1
                while [ ${CURVOL} -gt ${MINVOL} ]; do
                    # Slide down
                    NEWVOL=$((CURVOL - INC))
                    send setvol ${NEWVOL} >/dev/null 2>&1
                    #MPD_HOST=${MPD_HOST} mpc volume -${INC} >/dev/null 2>&1
                    CURVOL=$(curvol)
                    #CURVOL=$(MPD_HOST=${MPD_HOST} mpc volume | sed -e 's/[^0-9]//g')
                    if [ $((INCINC % 7)) -eq 0 -a ${INC} -lt 4 ]; then
                        INC=$((INC + 1))
                    fi
                    INCINC=$((INCINC + 1))
                done
            else
                INC=4
                while [ ${CURVOL} -lt ${MAXVOL} ]; do
                    # Slide up
                    NEWVOL=$((CURVOL + INC))
                    send volume ${CURVOL} >/dev/null 2>&1
                    #MPD_HOST=${MPD_HOST} mpc volume +${INC} >/dev/null 2>&1
                    CURVOL=$(curvol)
                    #CURVOL=$(MPD_HOST=${MPD_HOST} mpc volume | sed -e 's/[^0-9]//g')
                    if [ $((INCINC % 7)) -eq 0 -a ${INC} -gt 1 ]; then
                        INC=$((INC - 1))
                    fi
                    INCINC=$((INCINC + 1))
                done
            fi
            ;;
    esac
fi
