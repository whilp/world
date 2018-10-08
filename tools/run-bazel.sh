#!/bin/bash

set -euo pipefail

export BAZEL_PYTHON=/usr/bin/python2.7

main() {
    pid="$(bazel info server_pid)"
    out="$(bazel info output_base)/server/jvm.out"
    explain="/tmp/explain.out"

    report() {
        (
            set -x
            free -m
            df -h
            cat "$out"
            cat "$explain"
        )
    }
    trap report EXIT

    reap() {
        sleep 300
        kill -3 "$pid"
        sleep 5
        echo "REAPED BAZEL"
        bazel shutdown
    }

    run() {
        (
            set -x
            bazel \
                --output_base="$HOME/.cache/bazel" \
                test \
                --config=ci \
                "$@"
        )
    }

    (sleep 300 && reap) &
    run "$@"

    return $?
}

main "$@"
exit $?
