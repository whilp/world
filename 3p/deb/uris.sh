#!/bin/bash

set -euo pipefail

main() {
    apt-get update
    apt-get install -qq --print-uris \
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
        openssh-server
}

main "$@"
exit $?
