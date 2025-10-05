#!/bin/bash

export SRC="$PWD"
export DST="$HOME"
export PATH="$SRC/.local/bin:$PATH"

main() {
  _git
  _shell
  _shimlink
  _claude &
  _nvim &
  wait
}

_git() {
  cp -ra "$SRC/.git" "$DST/.git"
  (
    cd "$DST"
    git checkout .
    git config user.email 189851+whilp@users.noreply.github.com
  )
  command -v watchman >/dev/null 2>&1 && watchman watch-project "$DST"
}

_shell() {
  sudo chsh "$(id -un)" --shell $(which zsh)
  echo 'export SHELL=/bin/zsh' >"$DST/.bashrc"
}

_shimlink() {
  ast-grep --version
  biome --version
  claude --version
  comrak --version
  delta --version
  gh --version
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
  local claude=$(command -v claude 2>/dev/null)
  ln -sf "${claude:-shimlink}" "$DST/.local/bin/claude"

  local config="$DST/.claude.json"
  [ -r "$config" ] && return

  if [ -n "${CLAUDE_CREDENTIALS}" ]; then
    echo "${CLAUDE_CREDENTIALS}" >"$DST/.claude/.credentials.json"
  fi

  local auth=""
  if [ -n "${CLAUDE_API_KEY}" ]; then
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
  echo "$settings" >"$config"
}

_nvim() {
  # Install vim.pack plugins by starting nvim headlessly
  nvim --headless +'lua vim.wait(30000, function() return vim.fn.isdirectory(vim.fn.stdpath("data") .. "/site/pack/core/opt/mini.nvim") == 1 end)' +qa

  # Generate helptags
  nvim --headless +'helptags ALL' +qa

  # Enable and start nvim server service
  systemctl --user daemon-reload
  systemctl --user enable nvim.service
  systemctl --user start nvim.service
}

main "$@"
