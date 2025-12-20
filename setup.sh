#!/bin/bash

SETUP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.config/setup" && pwd)"

get_platform() {
  local os arch
  case "$(uname -s)" in
  Darwin) os="darwin" ;;
  Linux) os="linux" ;;
  *)
    echo "unsupported OS: $(uname -s)" >&2
    return 1
    ;;
  esac

  case "$(uname -m)" in
  arm64 | aarch64) arch="arm64" ;;
  x86_64 | amd64) arch="x86_64" ;;
  *)
    echo "unsupported architecture: $(uname -m)" >&2
    return 1
    ;;
  esac

  echo "${os}-${arch}"
}

install_home() {
  local platform binary_name url tmpdir
  platform=$(get_platform) || return 1
  binary_name="home-${platform}"
  url="https://github.com/whilp/dotfiles/releases/latest/download/${binary_name}"
  tmpdir=$(mktemp -d)

  echo "downloading ${binary_name}..." >&2
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
  "${home_bin}" unpack --force "$HOME"
  rm -rf "$(dirname "${home_bin}")"

  "$SETUP_DIR/luajit"

  export LUA_PATH="$SETUP_DIR/?.lua;$HOME/.local/bootstrap/lib/lua/?.lua;$HOME/.local/bootstrap/lib/lua/?/init.lua;;"
  lua -e "require('setup').main()"
}

main "$@"
