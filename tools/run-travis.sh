#!/bin/bash

set -euo pipefail

main() {
    bin="$1"
    set -x
    env
    command -v python
    PATH="$bin:$PATH" ./tools/run-bazel.sh "$@"
}

main "$@"
exit $?
