#!/bin/bash

set -euo pipefail

BIN="$HOME/bin"

main() {
    mode="${1:-}"
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
    install
    setup
    ./tools/run-bazel.sh run image
}

setup() {
    mkdir -p "$BIN"
    export PATH="$BIN:$PATH"
    ln -s "$(command -v python2.7)" "$BIN/python"
    ./tools/get-bazel.sh "$BIN/bazel"
}

install() {
    apt-get update && apt-get install -y \
        git \
        zip \
        unzip \
        g++ \
        python2.7 \
        python3 \
        python3-setuptools \
        python3-dev \
        curl
}

main "$@"
exit $?
