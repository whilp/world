#!/bin/bash

set -euo pipefail

main() {
    print_uris | ./3p/deb/write >"$BUILD_WORKSPACE_DIRECTORY"/3p/deb/debs.bzl
}

print_uris() {
    docker run --rm -ti ubuntu:18.04 /bin/bash -c \
        "apt-get update \
    && apt-get install --print-uris \
    libltdl7 \
    git \
    pkg-config \
    zip \
    g++ \
    zlib1g-dev \
    unzip \
    netcat-openbsd \
    python2.7 \
    python3 \
    python3-setuptools \
    python3-dev \
    python3-venv \
    curl \
    openssh-server \
    "
}

main "$@"
exit $?
