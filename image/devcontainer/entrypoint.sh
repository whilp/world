#!/usr/bin/env bash

set -euo pipefail

main() {
  dockerd-rootless.sh --experimental >"$XDG_RUNTIME_DIR/docker.log" 2>&1 &

  #sleep infinity
  exec "$@"
}

main "$@"
exit $?
