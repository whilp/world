#!/bin/bash

main() {
  setshell
  shimlinks
}

setshell() {
  sudo chsh "$(id -un)" --shell "/usr/bin/zsh"
}

shimlinks() {
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

main "$@"
