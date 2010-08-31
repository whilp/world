ps1 () {
    printf "\a${HOSTNAME}:$(tput bold)"
    case "${PWD}" in
        "${HOME}") printf '~';;
        "${HOME}"/*) printf '~'"${PWD#${HOME}}";;
        *) printf "${PWD}";;
    esac
    printf "$(tput sgr0) $"
}
PS1="\$(ps1) "
export PS1

set -o vi-tabcomplete
