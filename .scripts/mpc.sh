#!/bin/sh

PROMPT='mpc > '

while read INPUT?"${PROMPT}"; do
    case "x${INPUT}" in
        xclear)
            clear
            ;;
        xquit)
            exit
            ;;
        xhelp|x?)
            mpc help
            ;;
        xnp|xstatus)
            mpc status
            ;;
        *)
            CMD=${INPUT%% *}
            RESULT=$(mpc help | grep "^mpc ${CMD}" | cut -d ' ' -f 2)
            RESULTS=$(echo ${RESULT} | wc -w)
            if [ ${RESULTS} -eq 1 ]; then
                if [ $(echo ${INPUT} | wc -w) -eq 1 ]; then
                    DO=${RESULT}
                else
                    DO=${RESULT} ${INPUT#* }
                fi
                mpc ${DO}
            elif [ ${RESULTS} -gt 1 ]; then
                echo Ambiguous: ${RESULT}
            elif [ ${RESULTS} -lt 1 ]; then
                echo "Command not found: ${CMD}"
            fi
            ;;
    esac
done
