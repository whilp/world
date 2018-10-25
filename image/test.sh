#!/bin/bash

set -euo pipefail

main() {
    #test_image
    ./image/check
    return 0
}

test_image() {
    # Load the image.
    ./image/image

    iid="$(cat ./image/image.digest)"

    docker run --rm -t --entrypoint=/check "$iid"

    return $?
}

main "$@"
exit $?
