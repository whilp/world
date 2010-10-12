shorten () {
    typeset LENGTH=$1; shift
    typeset STRING="$*"
    typeset -L "$((${LENGTH} - 4))" NEW="${STRING}^"
    typeset NEWSTRING="${NEW}"
    NEWSTRING="${NEWSTRING%%^ *}"
    NEWSTRING="${NEWSTRING%^}"
    [ "${NEWSTRING}" = "${STRING}" ] || NEWSTRING="${NEWSTRING}..."
    echo "${NEWSTRING}"
}

ps1 () {
    typeset PS1PWD=
    typeset PWDCOMPONENTS=
    printf "\a${HOSTNAME}:$(tput bold)"
    case "${PWD}" in
        "${HOME}") PS1PWD='~';;
        "${HOME}"/*) PS1PWD='~'"${PWD#${HOME}}";;
        *) PS1PWD="${PWD}";;
    esac

    typeset OLDPS1PWD=${PS1PWD}
    IFS=/ set -A PWDCOMPONENTS "${PS1PWD}"
    PS1PWD=
    typeset i=0
    typeset COMPONENT=
    typeset MAX="${MAXPS1PWDCOMP:-5}"
    for COMPONENT in ${PWDCOMPONENTS}; do
        i=$(($i + 1))
        if [ $i -ge "${MAX}" ]; then
            typeset OLDLAST="${OLDPS1PWD#${PS1PWD}}"
            typeset LAST=${OLDLAST##*/}
            case "${LAST}" in
                "") COMPONENT="";;
                "${OLDLAST}") COMPONENT="${LAST}";;
                *) COMPONENT=".../${LAST}";;
            esac
            PS1PWD="${PS1PWD}${COMPONENT}/"
            break
        fi
        PS1PWD="${PS1PWD}${COMPONENT}/"
    done

    case "${USER}" in
        root) DOLLAR="#";;
        *) DOLLAR="$";;
    esac

    printf "$(shorten 20 ${PS1PWD%/})$(tput sgr0) ${DOLLAR}"
}

PS1="\$(ps1) "
export PS1

set -o vi-tabcomplete
