#!/bin/bash

set -euo pipefail

main() {
    apt-get update
    apt-get install \
        -oAcquire::ForceHash=sha256 \
        -qq \
        --print-uris \
        --no-install-recommends \
        "${debs[@]}"
}

debs=(
    curl
    g++
    git
    libltdl7
    netcat-openbsd
    openssh-server
    pkg-config
    python-pip
    python2.7-dev
    python2.7-minimal
    python3-pip
    python3-setuptools
    python3.7-dev
    python3.7-minimal
    unzip
    zip
    zlib1g-dev
)

main "$@"
exit $?
