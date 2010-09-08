ps1 () {
    local PS1PWD=
    local PWDCOMPONENTS=
    printf "\a${HOSTNAME}:$(tput bold)"
    case "${PWD}" in
        "${HOME}") PS1PWD='~';;
        "${HOME}"/*) PS1PWD='~'"${PWD#${HOME}}";;
        *) PS1PWD="${PWD}";;
    esac

    local OLDPS1PWD=${PS1PWD}
    IFS=/ set -A PWDCOMPONENTS ${PS1PWD}
    PS1PWD=
    local i=0
    local COMPONENT=
    local MAX="${MAXPS1PWDCOMP:-5}"
    for COMPONENT in ${PWDCOMPONENTS}; do
        i=$(($i + 1))
        if [ $i -ge "${MAX}" ]; then
            local OLDLAST="${OLDPS1PWD#${PS1PWD}}"
            local LAST=${OLDLAST##*/}
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

    printf "${PS1PWD%/}$(tput sgr0) $"
}

PS1="\$(ps1) "
export PS1

set -o vi-tabcomplete
