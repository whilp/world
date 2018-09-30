#!/bin/bash

set -euo pipefail

main() {
    mode="$1"
    case "$mode" in
    __inner__) inner ;;
    *) outer ;;
    esac
}

outer() {
    docker run --rm \
        --volume "$PWD":/work \
        --volume /var/run/docker.sock:/var/run/docker.sock \
        --workdir "/work" \
        ubuntu \
        ./tools/bootstrap.sh __inner__
    return $?
}

inner() {
    export PATH="$HOME/bin:$PATH"
    setup
    bazel run image
}

setup() {
    ln -s "$(command -v python2)" "$HOME/bin/python"
    ./tools/get-bazel.sh "$HOME/bin/bazel"
}

main "$@"
exit $?
