#!/bin/bash

set -euo pipefail

main() {
    bin="$1"
    set -x
    bins=(
        "$bin"
        # TODO
        #/usr/local/clang-5.0.0/bin
        /usr/local/sbin
        /usr/local/bin
        /usr/sbin
        /usr/bin
        /sbin
        /bin
    )
    path="$(join : "${bins[@]}")"
    PATH="$path" ./tools/run-bazel.sh "$@"
}

join() {
    local IFS="$1"
    shift
    echo "$*"
}

main "$@"
exit $?
