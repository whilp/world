#!/bin/bash

set -euo pipefail

main() {
    ./image/image
    return 1
}

main "$@"
exit $?
