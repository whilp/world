#!/bin/bash

set -euo pipefail

main() {
    ./image/image
}

main "$@"
exit $?
