#!/bin/bash

set -euo pipefail

main() {
    apt-get update
    apt-get install -oAcquire::ForceHash=sha256 -qq --print-uris "${debs[@]}"
}

debs=(
    curl
    g++
    git
    libltdl7
    netcat-openbsd
    openssh-server
    pkg-config
    python2.7
    python3
    python3-dev
    python3-setuptools
    python3-venv
    unzip
    zip
    zlib1g-dev
)

main "$@"
exit $?
