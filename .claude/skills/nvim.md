---
description: Interact with Neovim configuration and service
---

# Neovim interaction skill

This skill provides comprehensive knowledge for interacting with Neovim in this environment.

## Configuration

- Use Neovim with configuration in plugin directory at `.config/nvim/plugin/`
- Prefer Lua for Neovim configuration
- Only use the nvim native package manager (available in 0.12+) to install plugins
- Generally avoid plugins and prefer minimal, native configurations
- Use `<Space>` instead of `<leader>` in nvim keybindings

## Wrapper script

- nvim wrapper script at `.local/bin/nvim-1` handles client/server mode automatically
- Uses socket at `.config/nvim/nvim.sock` for client connections

## Remote inspection

Inspect running nvim server with: `nvim --remote-expr "expression"`

Useful expressions:
- `get(b:, 'term_title', 'unset')` - get terminal title
- `&title` - get title option
- `&titlestring` - get titlestring option

Important notes:
- Use `execute()` when sourcing the nvim config
- The vim global isn't available in remote-expr, so use vimscript instead

## Native package manager (vim.pack)

Install plugins by adding them to configuration files with `vim.pack.add()`

Key details:
- Plugins are stored in `$XDG_DATA_HOME/nvim/site/pack/core/opt`
- Basic syntax: `vim.pack.add({ { src = "https://github.com/user/plugin", version = vim.version.range("1.0.0") } })`
- Use version constraints for stability: `version = vim.version.range("2.4.0")`
- Check if plugin loaded before using: `local ok, plugin = pcall(require, "plugin-name")`

## Service management

### systemd (Linux)

User services are in `~/.config/systemd/user/`

Common commands:
- Reload after changes: `systemctl --user daemon-reload`
- Enable at login: `systemctl --user enable nvim`
- Start now: `systemctl --user start nvim`
- Check status: `systemctl --user status nvim`
- View logs: `journalctl --user -u nvim -f`

### launchd (macOS)

User services are in `~/Library/LaunchAgents/`

Common commands:
- Load and start: `launchctl load ~/Library/LaunchAgents/com.user.nvim.plist && launchctl start com.user.nvim`
- Load service: `launchctl load ~/Library/LaunchAgents/com.user.nvim.plist`
- Unload service: `launchctl unload ~/Library/LaunchAgents/com.user.nvim.plist`
- Start service: `launchctl start com.user.nvim` (must load first)
- Stop service: `launchctl stop com.user.nvim`
- Check status: `launchctl list | grep nvim`
- View logs: `log stream --predicate 'process == "nvim"'`
