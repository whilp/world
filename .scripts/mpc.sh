#!/bin/sh

PROMPT='mpc > '

while read INPUT?"${PROMPT}"; do
    case "x${INPUT}" in
        x)
            clear
            mpc status
            ;;
        xx)
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
            mpc $INPUT
            ;;
    esac
done
