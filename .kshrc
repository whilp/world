ps1 () {
    printf "\a${HOSTNAME} "
    case "${PWD}" in
        "${HOME}") printf '~';;
        "${HOME}"/*) printf '~'"${PWD#${HOME}}";;
        *) printf "${PWD}";;
    esac
    printf " $"
}
PS1="\$(ps1) "
export PS1

set -o vi
