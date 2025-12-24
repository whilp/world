#!/bin/bash

SETUP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.config/setup" && pwd)"

install_home() {
  local url tmpdir
  url="https://github.com/whilp/dotfiles/releases/latest/download/home"
  tmpdir=$(mktemp -d)

  echo "downloading home..." >&2
  if ! curl -fsSL -o "${tmpdir}/home" "${url}"; then
    echo "failed to download home binary" >&2
    rm -rf "${tmpdir}"
    return 1
  fi

  chmod +x "${tmpdir}/home"
  echo "${tmpdir}/home"
}

main() {
  home_bin=$(install_home) || return 1
  "${home_bin}" unpack --force --with-platform "$HOME"
  rm -rf "$(dirname "${home_bin}")"

  export LUA_PATH="$SETUP_DIR/?.lua;$HOME/.local/bootstrap/lib/lua/?.lua;$HOME/.local/bootstrap/lib/lua/?/init.lua;;"
  lua -e "require('setup').main()"
}

main "$@"
