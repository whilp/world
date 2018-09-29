#!/bin/bash

set -eou pipefail

read_value() {
	file="$1"
	key="$2"
	sed -ne "s/$key = \"\\(.*\\)\"/\\1/p" "$file"
}

BAZEL_SHA256=$(read_value image/files.bzl BAZEL_SHA256)
BAZEL_URL=$(read_value image/files.bzl BAZEL_URL)

cat <<EOF
STABLE_HEAD $(git rev-parse HEAD)
STABLE_BRANCH $(git rev-parse --abbrev-ref HEAD)
STABLE_BAZEL_SHA256 ${BAZEL_SHA256}
STABLE_BAZEL_URL ${BAZEL_URL}
EOF
