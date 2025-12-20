#!/bin/bash

SETUP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.config/setup" && pwd)"

get_platform() {
  local os arch
  case "$(uname -s)" in
    Darwin) os="darwin" ;;
    Linux) os="linux" ;;
    *) echo "unsupported OS: $(uname -s)" >&2; return 1 ;;
  esac

  case "$(uname -m)" in
    arm64|aarch64) arch="arm64" ;;
    x86_64|amd64) arch="x86_64" ;;
    *) echo "unsupported architecture: $(uname -m)" >&2; return 1 ;;
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

if command -v home &>/dev/null; then
  home unpack
else
  home_bin=$(install_home) || exit 1
  "${home_bin}" unpack
  rm -rf "$(dirname "${home_bin}")"
fi

main() {
  "$SETUP_DIR/backup"
  "$SETUP_DIR/git"
  "$SETUP_DIR/shell"
  "$SETUP_DIR/luajit"
  "$SETUP_DIR/shimlink"
  "$SETUP_DIR/codespace"
  "$SETUP_DIR/claude" &
  "$SETUP_DIR/nvim" &
  "$SETUP_DIR/extras" &
  "$SETUP_DIR/ai" &
  wait
}

main "$@"
