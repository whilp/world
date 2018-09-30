#!/bin/bash

set -euo pipefail

export BAZEL_PYTHON=/usr/bin/python2.7

main() {
    bazel="$1"
    shift
    (
        set -x
        $bazel \
            --output_base="$HOME/.cache/bazel" \
            --host_jvm_args=-Xmx500m \
            --host_jvm_args=-Xms500m \
            test \
            --config=ci \
            --experimental_repository_cache="$HOME/.bazel_repository_cache" \
            --local_resources=400,1,1.0 \
            "$@"
    )
    return $?
}

main "$@"
exit $?
