#!/bin/bash

set -euo pipefail

main() {
    buildifier="$1"
    target="$BUILD_WORKSPACE_DIRECTORY"/3p/deb/repo.bzl

    # TODO: the digest here should work but it does not.
    #digest=$(cat ./3p/deb/uris.digest)
    digest=bazel/3p/deb:uris
    # Help the loader find its runfiles.
    PYTHON_RUNFILES=${BASH_SOURCE[0]}.runfiles ./3p/deb/uris
    docker run --rm -ti "$digest" |
        ./3p/deb/write >"$target"
    $buildifier "$target"
}

main "$@"
exit $?
