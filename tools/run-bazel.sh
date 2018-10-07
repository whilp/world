#!/bin/bash

set -euo pipefail

export BAZEL_PYTHON=/usr/bin/python2.7

main() {
    bazel="$1"
    shift

    pid="$("$bazel" info server_pid)"
    out="$("$bazel" info output_base)/server/jvm.out"

    report() {
        (
            set -x
            free -m
            df -h
        )
    }
    trap report EXIT

    reap() {
        sleep 300
        kill -3 "$pid"
        sleep 5
        echo "REAPED BAZEL; contents of $out"
        cat "$out"
        "$bazel" shutdown
    }

    run() {
        (
            set -x
            "$bazel" \
                --output_base="$HOME/.cache/bazel" \
                test \
                --config=ci \
                "$@"
        )
    }

    report
    (sleep 300 && reap) &
    run

    return $?
}

main "$@"
exit $?
