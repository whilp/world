#!/bin/bash

detect_platform() {
  local os arch
  os=$(uname -s | tr '[:upper:]' '[:lower:]')
  arch=$(uname -m)

  case "${os}" in
    darwin)
      os="darwin"
      ;;
    linux)
      os="linux"
      ;;
    *)
      echo "unsupported os: ${os}" >&2
      return 1
      ;;
  esac

  case "${arch}" in
    x86_64|amd64)
      arch="x86_64"
      ;;
    arm64|aarch64)
      arch="arm64"
      ;;
    *)
      echo "unsupported architecture: ${arch}" >&2
      return 1
      ;;
  esac

  echo "${os}-${arch}"
}

install_home() {
  local platform url tmpdir
  platform=$(detect_platform) || return 1
  url="https://github.com/whilp/world/releases/latest/download/home-${platform}"
  tmpdir=$(mktemp -d)

  echo "downloading home for ${platform}..." >&2
  if ! curl -fsSL -o "${tmpdir}/home" "${url}"; then
    echo "failed to download home binary for ${platform}" >&2
    rm -rf "${tmpdir}"
    return 1
  fi

  chmod +x "${tmpdir}/home"
  echo "${tmpdir}/home"
}

main() {
  home_bin=$(install_home) || return 1
  "${home_bin}" unpack --force --verbose "$HOME"
  "${home_bin}" setup --verbose "$HOME"
  rm -rf "$(dirname "${home_bin}")"
}

main "$@"
