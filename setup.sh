#!/bin/bash

export SRC="$PWD"
export DST="$HOME"
export PATH="$SRC/.local/bin:$PATH"
export SHELLINIT="$DST/.config/shellinit"

main() {
  _backup
  _git
  _shell
  _luajit
  _shimlink
  _claude &
  _nvim &
  wait
}

_backup() {
  mkdir -p "$SHELLINIT"
  (
    cd "$DST"
    for name in bashrc bash_profile profile zshrc; do
      [[ -r ".$name" ]] && cp ".$name" "$SHELLINIT/$name"
    done
  )
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

_luajit() {
  # Detect platform and architecture
  local os=$(uname -s | tr '[:upper:]' '[:lower:]')
  local arch=$(uname -m)
  local url

  case "$os-$arch" in
  linux-x86_64)
    url="https://github.com/whilp/dotfiles/releases/download/luajit-3/luajit-2025.10.16-25a61a18-linux-x64.tar.gz"
    ;;
  linux-aarch64 | linux-arm64)
    url="https://github.com/whilp/dotfiles/releases/download/luajit-3/luajit-2025.10.16-25a61a18-linux-arm64.tar.gz"
    ;;
  darwin-arm64)
    url="https://github.com/whilp/dotfiles/releases/download/luajit-3/luajit-2025.10.16-25a61a18-darwin-arm64.tar.gz"
    ;;
  *)
    echo "Unsupported platform: $os-$arch" >&2
    exit 1
    ;;
  esac

  # Create temporary directory for bootstrap
  local temp_dir=$(mktemp -d)
  local archive="$temp_dir/luajit.tar.gz"

  # Download LuaJIT
  echo "Bootstrapping LuaJIT from $url" >&2
  curl -fsSL -o "$archive" "$url"

  # Extract to temporary location
  tar -xzf "$archive" -C "$temp_dir"

  # Find the extracted directory (it will have the version in the name)
  local luajit_dir=$(find "$temp_dir" -maxdepth 1 -type d -name "luajit-*" | head -n1)

  # Export the bootstrapped LuaJIT path and LUA_PATH
  export BOOTSTRAP_LUAJIT="$luajit_dir/bin/luajit"
  export LUA_PATH="$DST/.local/lib/lua/?.lua;$DST/.local/lib/lua/?/init.lua;;"

  # Create initial lua-shimlink symlink for shimlink's shebang
  mkdir -p "$DST/.local/bin"
  ln -sf "$BOOTSTRAP_LUAJIT" "$DST/.local/bin/lua-shimlink"

  echo "Bootstrapped LuaJIT at $BOOTSTRAP_LUAJIT" >&2
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
  luajit --version
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
  NVIM_INVIM=1 nvim-1 --headless +'lua vim.wait(30000, function() return vim.fn.isdirectory(vim.fn.stdpath("data") .. "/site/pack/core/opt/mini.nvim") == 1 end)' +qa

  # Generate helptags
  NVIM_INVIM=1 nvim-1 --headless +'helptags ALL' +qa

  # Load and start nvim server service
  nvim load
  nvim restart
}

main "$@"
