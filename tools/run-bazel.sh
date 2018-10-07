#!/bin/bash

set -euo pipefail

export BAZEL_PYTHON=/usr/bin/python2.7

main() {
    bazel="$1"
    shift
    reap "$bazel" &
    (
        set -x
        "$bazel" \
            --output_base="$HOME/.cache/bazel" \
            test \
            --config=ci \
            "$@"
    )
    return $?
}

reap() {
    bazel="$1"
    sleep 200
    kill -3 "$("$bazel" info server_pid)" &
}

main "$@"
exit $?
