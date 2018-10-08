#!/bin/bash

set -euo pipefail

main() {
    bin="$1"
    shift
    set -x
    mkdir -p "$bin"
    ln -s "$(command -v python2)" "$bin/python"
    ./tools/install-bazel.sh "$bin/bazel"
}

main "$@"
exit $?
