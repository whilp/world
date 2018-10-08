#!/bin/bash

set -euo pipefail

main() {
    ./image/image
    return $?
}

main "$@"
exit $?
