#!/usr/bin/env bash

set -euo pipefail

main() {
  set -x
  home="/home/user"
  export XDG_RUNTIME_DIR="$home/.xdg"
  export DOCKER_HOST="unix:///$XDG_RUNTIME_DIR/docker.sock"

  sudo mkdir -p "$home"
  sudo mkdir -p "$XDG_RUNTIME_DIR"
  sudo chown -R user:user "$home"

  dockerd-rootless.sh --experimental >"$XDG_RUNTIME_DIR/docker.log" 2>&1 &

  #sleep infinity
  exec "$@"
}

main "$@"
exit $?
