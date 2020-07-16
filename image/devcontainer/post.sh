#!/usr/bin/env bash

set -euo pipefail

main() {
  sudo chgrp /run/docker-host.sock
  exec "$@"
}

main "$@"
exit $?
