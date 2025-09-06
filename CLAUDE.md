## Writing

- Always use sentence case

## Scripts

- Store scripts in `.local/bin`

## Terminal

- Alacritty config is located at `.config/alacritty/alacritty.toml`

## Neovim

- Use Neovim with configuration in plugin directory at `.config/nvim/plugin/`
- Prefer Lua for Neovim configuration
- nvim wrapper script at `.local/bin/nvim-1` handles client/server mode automatically
- uses socket at `.config/nvim/nvim.sock` for client connections
- inspect running nvim server with: `nvim --remote-expr "expression"`
- useful expressions: `get(b:, 'term_title', 'unset')`, `&title`, `&titlestring`
- use execute() when sourcing the nvim config
- the vim global isn't available in remote-expr, so use vimscript instead
- only use the nvim native package manager (available in 0.12+) to install plugins. generally avoid plugins and prefer minimal, native configurations

### Using vim.pack Native Package Manager

- install plugins by adding them to configuration files with `vim.pack.add()`
- plugins are stored in `$XDG_DATA_HOME/nvim/site/pack/core/opt`
- basic syntax: `vim.pack.add({ { src = "https://github.com/user/plugin", version = vim.version.range("1.0.0") } })`
- use version constraints for stability: `version = vim.version.range("2.4.0")`
- check if plugin loaded before using: `local ok, plugin = pcall(require, "plugin-name")`
- use <Space> instead of <leader> in nvim keybindings

## SQL

- always write sql in lowercase style for trino

## Searching

- ALWAYS use `rg` (ripgrep) to explore files
- NEVER use `find`
- NEVER use `grep`
- to find files with `rg`: `rg --files -g '*.<extension>' <path>`; if this doesn't work, consider whether it is appropriate to ignore gitignore and then try again with `-uuu`

## Python

- ALWAYS invoke `python3`
- NEVER invoke plain `python`

## Docs

- ALWAYS fetch the .md version of docs; for <https://docs.anthropic.com/en/docs/claude-code/hooks> -\> <https://docs.anthropic.com/en/docs/claude-code/hooks.md>

## Gists

- to create a gist, do `gh create <file> --desc "<description>"`
