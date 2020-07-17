#!/usr/bin/env bash

set -euo pipefail

main() {
  exec "$@"
}

main "$@"
exit $?
