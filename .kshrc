BOLD=$(tput bold)
UNBOLD=$(tput sgr0)

ps1 () {
    if [ -n "$*" ]; then
        ARG=$1; OPTARG=$2
        case "${ARG}" in
            -s) export PS1SESSION="${OPTARG}";;
            -l) case "${OPTARG}" in
                    [+-]*) MAXPS1LEN="$((${MAXPS1LEN:-0} + ${OPTARG}))";;
                    *) MAXPS1LEN="${OPTARG}";;
                esac;;
        esac
        return 0
    fi

    typeset PS1PWD=
    typeset PS1SESSION="${PS1SESSION:-${HOSTNAME}}"
    typeset MAXPS1LEN=$((${MAXPS1LEN:-20} - 5 - ${#PS1SESSION}))
    case "${PWD}" in
        "${HOME}") PS1PWD='~';;
        "${HOME}"/*) PS1PWD='~'"${PWD#${HOME}}";;
        *) PS1PWD="${PWD}";;
    esac

    typeset TILDE=
    if [ -z "${PS1PWD%%\~*}" ]; then
        TILDE="~"
        PS1PWD="${PS1PWD#\~}"
        MAXPS1LEN=$((${MAXPS1LEN} - 1))
    fi
    typeset LAST="${PS1PWD##*/}"
    typeset TRUNCATE=
    if [ "${#LAST}" -gt "${MAXPS1LEN}" ]; then
        # Truncate.
        typeset -R"$((${MAXPS1LEN} + 1))" TMPLAST="${LAST}"
        TRUNCATE=1
        LAST="${TMPLAST#?}"
    else
        while [ -n "${PS1PWD}" -a "${#LAST}" -lt "${MAXPS1LEN}" ]; do
            PS1PWD="${PS1PWD%/*}"
            LAST="${PS1PWD##*/}/${LAST#/}"
        done
        typeset -R"${MAXPS1LEN}" TMPLAST="${LAST}"
        if [ -n "${PS1PWD}" ]; then
            LAST="${TMPLAST}"
            TRUNCATE=1
        fi
    fi
    if [ -n "${TRUNCATE}" ]; then
        TRUNCATE="${UNBOLD}<${BOLD}"
        TILDE=
    fi
    PS1PWD="${TRUNCATE}${TILDE}${LAST%/}"
    typeset DOLLAR="$"
    case "${USER}" in root) DOLLAR="#";; esac

    echo -ne "\a${PS1SESSION}:${BOLD}${PS1PWD}${UNBOLD} ${DOLLAR}"
}

PS1="\$(ps1) "
export PS1

set -o vi-tabcomplete
