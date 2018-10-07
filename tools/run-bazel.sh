#!/bin/bash

set -euo pipefail

export BAZEL_PYTHON=/usr/bin/python2.7

main() {
    bazel="$1"
    shift
    pid="$("$bazel" info server_pid)"
    out="$("$bazel" info output_base)/server/jvm.out"
    reap "$pid" "$out" &
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
    pid="$1"
    sleep 200
    kill -3 "$pid"
    sleep 5
    echo "REAPED BAZEL; contents of $out"
    cat "$out"
}

main "$@"
exit $?
