#!/bin/bash

export SRC="$PWD"
export DST="$HOME"
export PATH="$SRC/.local/bin:$PATH"

main() {
  _git
  _shell
  _shimlink &
  _claude &
  wait
}

_git() {
  cp -ra "$SRC/.git" "$DST/.git"
  cd "$DST" && git checkout .
}

_shell() {
  sudo chsh "$(id -un)" --shell "/usr/bin/zsh"
}

_shimlink() {
  ast-grep --version
  biome --version
  comrak --version
  marksman --version
  nvim --version
  rg --version
  ruff --version
  shfmt --version
  sqruff --version
  stylua --version
  superhtml version
  tree-sitter --version
  uv --version
}

_claude() {
  local claude="$DST/.claude"
  local credentials="$claude/.credentials.json"
  mkdir -p "$claude"
  [ -r "$credentials" ] || echo "$CLAUDE_CREDENTIALS" >"$credentials"
  which claude >/dev/null 2>&1 && return
  curl -fsSL https://claude.ai/install.sh | bash -s latest
}

main "$@"
