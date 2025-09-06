#!/bin/bash

main() {
  _shell
  _shimlink
  _claude
}

_shell() {
  sudo chsh "$(id -un)" --shell "/usr/bin/zsh"
}

_shimlink() {
  ast-grep --version &
  biome --version &
  comrak --version &
  marksman --version &
  nvim --version &
  ruff --version &
  shfmt --version &
  sqruff --version &
  stylua --version &
  superhtml --version &
  tree-sitter --version &
  uv --version &
}

_claude() {
  local credentials=~/.claude/.credentials.json
  [ -r "$credentials" ] || echo "$CLAUDE_CREDENTIALS" >"$credentials"
  which claude >/dev/null 2>&1 && return
  curl -fsSL https://claude.ai/install.sh | bash -s latest
}

main "$@"
