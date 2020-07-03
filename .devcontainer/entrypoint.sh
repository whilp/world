#!/usr/bin/env bash

set -euo pipefail

main() {
  set -x
  sudo mkdir -p /home/user
  sudo chown user:user /home/user
  #sleep infinity
  exec "$@"
}

main "$@"
exit $?
