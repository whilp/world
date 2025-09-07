#!/bin/bash

export SRC="$PWD"
export DST="$HOME"
export PATH="$SRC/.local/bin:$PATH"

main() {
  _git
  _shell &
  _shimlink &
  _claude &
  wait
}

_git() {
  cp -ra "$SRC/.git" "$DST/.git"
  cd "$DST" && git checkout .
}

_shell() {
  sudo chsh "$(id -un)" --shell $(which zsh)
}

_shimlink() {
  ast-grep --version
  biome --version
  claude --version
  comrak --version
  delta --version
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
  local claude="$DST/.claude.json"
  [ -r "$claude" ] && return

  local auth=""
  if [ -n "CLAUDE_API_KEY" ]; then
    auth='"primaryApiKey": "'${CLAUDE_API_KEY}'",'
  fi

  local settings='{
  "numStartups": 1,
  "installMethod": "unknown",
  "autoUpdates": true,
  "theme": "dark-daltonized",
  '${auth}'
  "hasCompletedOnboarding": true
}'
  echo "$settings" >"$claude"
}

main "$@"
