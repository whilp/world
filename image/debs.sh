#!/bin/bash

set -euo pipefail

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
    python2.7 \
    python3 \
    python3-setuptools \
    python3-dev \
    curl \
    " \
    | python image/debs.py > image/debs.bzl
