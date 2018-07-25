#!/bin/bash

set -eou pipefail

cat <<EOF
STABLE_HEAD $(git rev-parse HEAD)
STABLE_BRANCH $(git rev-parse --abbrev-ref HEAD)
EOF
