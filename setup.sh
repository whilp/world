#!/bin/bash

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
  "${home_bin}" unpack --force --with-platform --verbose "$HOME"
  "${home_bin}" setup --verbose "$HOME"
  rm -rf "$(dirname "${home_bin}")"
}

main "$@"
