#!/bin/bash

export SRC="$PWD"
export DST="$HOME"
export PATH="$DST/.local/share/shimlink/bin:$DST/.local/bin:$DST/extras/bin:$PATH"
export SHELLINIT="$DST/.config/shellinit"
export REMOTE=$(
  git config --get remote.origin.url
)

main() {
  _backup
  _git
  _shell
  _luajit
  _shimlink
  _claude
  _nvim
  _extras
  _ai
}

_backup() {
  mkdir -p "$SHELLINIT"
  (
    cd "$DST"
    for name in bashrc bash_profile profile zshrc; do
      [[ -r ".$name" && ! -f "$SHELLINIT/$name" ]] && cp ".$name" "$SHELLINIT/$name"
    done
  )
}

_git() {
  rm -rf "$DST/.git"
  cp -ra "$SRC/.git" "$DST/.git"
  (
    cd "$DST"
    git checkout .
    git config user.email 189851+whilp@users.noreply.github.com
    git config core.fsmonitor false
  )
  command -v watchman >/dev/null 2>&1 && watchman watch-project "$DST"
}

_extras() {
  local extras=${REMOTE%/*}/extras
  (
    cd "$DST"
    if [ ! -d extras ]; then
      git clone "$extras" extras
    else
      cd ./extras
      git fetch
    fi
    cd extras
    [ -x ./setup.sh ] && ./setup.sh
  )
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
  # Also create lua symlink for tools that expect plain 'lua'
  ln -sf "$BOOTSTRAP_LUAJIT" "$DST/.local/bin/lua"

  echo "Bootstrapped LuaJIT at $BOOTSTRAP_LUAJIT" >&2
}

_shimlink() {
  shimlink update ast-grep
  shimlink update biome
  shimlink update comrak
  shimlink update delta
  shimlink update gh
  shimlink update marksman
  shimlink update nvim
  shimlink update rg
  shimlink update ruff
  shimlink update shfmt
  shimlink update sqruff
  shimlink update stylua
  shimlink update superhtml
  shimlink update tree-sitter
  shimlink update uv
  shimlink update luajit
}

_claude() {
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
  # Clean up any stale swap files
  rm -rf "$DST/.local/state/nvim/swap"

  # Install vim.pack plugins by starting nvim headlessly
  NVIM_INVIM=1 nvim-1 --headless +'lua vim.wait(30000, function() return vim.fn.isdirectory(vim.fn.stdpath("data") .. "/site/pack/core/opt/mini.nvim") == 1 end)' +qa

  # Generate helptags
  NVIM_INVIM=1 nvim-1 --headless +'helptags ALL' +qa

  # Load and start nvim server service
  nvimd reload
  nvimd restart
}

_ai() {
  (
    cd "$DST"
    if [ ! -d ./ai ]; then
      git clone ${REMOTE%/*}/ai ./ai
    else
      (
        cd ./ai
        git fetch
      )
    fi

    claude plugin marketplace add ./ai
  )
}

main "$@"
