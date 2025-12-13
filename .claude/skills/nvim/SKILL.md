---
description: Interact with Neovim configuration and build system
trigger: Use this skill for neovim configuration, plugin management, or building custom nvim releases
---

# Neovim interaction skill

This skill provides comprehensive knowledge for interacting with Neovim in this environment.

## Architecture

### Binary management
- Actual nvim binary managed by shimlink at `~/.local/share/shimlink/bin/nvim`
- Custom nvim builds with bundled plugins available via `.github/workflows/build-nvim.yml`

### Configuration structure
- Entry point: `.config/nvim/init.lua` (sets up lua paths)
- Plugin configs: `.config/nvim/plugin/` (modular lua files)
  - editing.lua, git.lua, grep.lua, interface.lua, lsp.lua, mini.lua
  - system.lua, tab.lua, terminal.lua, treesitter.lua, window.lua
- Treesitter queries: `.config/nvim/queries/`

## Configuration guidelines

- Prefer Lua for all Neovim configuration
- Use modular approach: separate files in `.config/nvim/plugin/` for different features
- Use `<Space>` instead of `<leader>` for keybindings
- Generally avoid plugins; prefer minimal, native configurations

## Native package manager (vim.pack)

Install plugins using `vim.pack.add()` in plugin config files (nvim 0.12+):

```lua
vim.pack.add({
  { src = "https://github.com/user/plugin" },
})
```

Key details:
- Plugins stored in `$XDG_DATA_HOME/nvim/site/pack/core/opt`
- Optional version constraints: `version = vim.version.range("1.0.0")`
- Check if loaded: `local ok, plugin = pcall(require, "plugin-name")`

Example from mini.lua:
```lua
vim.pack.add({
  { src = "https://github.com/nvim-mini/mini.nvim" },
})

local ok_bufremove, _ = pcall(require, "mini.bufremove")
if ok_bufremove then
  require("mini.bufremove").setup()
end
```

## Building custom nvim releases

The repository includes infrastructure to build nvim nightly with bundled plugins:

### Build system components
- `scripts/build-nvim.lua`: lua script that downloads nvim nightly, bundles plugins, and creates tarballs
- `.config/nvim/nvim-pack-lock.json`: lock file defining plugin versions
- `.github/workflows/build-nvim.yml`: workflow to build for darwin-arm64, linux-arm64, linux-x64
- requires luajit with dkjson (via `.config/setup/luajit` bootstrap)

### Lock file format
```json
{
  "plugins": {
    "plugin-name": {
      "src": "https://github.com/user/plugin",
      "rev": "commit-hash-or-tag"
    }
  }
}
```

### Build process
1. Downloads nvim nightly from neovim/neovim releases
2. Clones each plugin from lock file at specified revision
3. Installs plugins to `share/nvim/site/pack/core/opt/`
4. Generates helptags with `nvim --headless +'helptags ALL' +qa`
5. Creates reproducible tarball with checksums
6. Verifies plugins load correctly

### Running the build
```bash
# Locally (requires luajit with dkjson)
bash scripts/build-nvim

# Via GitHub workflow
gh workflow run build-nvim.yml -f create_release=false  # test build
gh workflow run build-nvim.yml -f release_tag=2025.11.23  # create release
```

## Nvim wrapper and daemon management

The nvim wrapper at `~/.local/bin/nvim` provides automatic server management with flexible socket configuration.

### Socket configuration
- Default socket: `~/.config/nvim/nvim.sock`
- Specify via CLI: `nvim --socket /path/to/socket.sock`
- Specify via environment: `NVIM_SOCKET=/path/to/socket.sock nvim`
- Priority: CLI flag > env var > default

### Client mode (nvim)
Automatically starts a daemon server if not running, then connects:
```bash
nvim file.txt                                    # uses default socket, auto-starts if needed
nvim --socket /tmp/project.sock file.txt        # uses custom socket
nvim --remote-expr "execute('echo 42')"          # remote commands work too
```

### Daemon management (nvimd)
Explicit daemon control commands:
```bash
nvimd start                                      # start server at default socket
nvimd --socket /tmp/project.sock start           # start at custom socket
nvimd stop                                       # stop server
nvimd status                                     # check if running
nvimd restart                                    # restart server
nvimd cleanup                                    # remove stale socket file
```

### Multiple servers
Each socket path gets its own daemon, pidfile, and logfile:
- Socket: `/path/to/foo.sock`
- Pidfile: `/path/to/foo.pid`
- Logfile: `/path/to/foo.log`

### Reloading configuration

To reload nvim configuration after making changes:

```bash
nvim --remote-expr "execute('source ~/.config/nvim/init.lua')"
# or with custom socket:
nvim --socket /tmp/project.sock --remote-expr "execute('source ~/.config/nvim/init.lua')"
```

This sources the configuration in running nvim instances without restarting them.
