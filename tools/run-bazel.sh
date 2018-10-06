#!/bin/bash

set -euo pipefail

export BAZEL_PYTHON=/usr/bin/python2.7

main() {
  bazel="$1"
  shift
  (
    set -x
    $bazel \
      --output_base="$HOME/.cache/bazel" \
      test \
      --config=ci \
      "$@"
  )
  return $?
}

main "$@"
exit $?
