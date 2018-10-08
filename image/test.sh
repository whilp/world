#!/bin/bash

set -euo pipefail

main() {
    # TODO: debug
    tar tvzf ./image/image.tar
    return 1
}

main "$@"
exit $?
