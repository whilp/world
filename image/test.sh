#!/bin/bash

set -euo pipefail

main() {
    # TODO: debug
    #./image/image
    tar tvzf ./image/hack.tar
    return 1
}

main "$@"
exit $?
