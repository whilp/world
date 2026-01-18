# home: always bundle nvim with plugins

Makes nvim always include bundled plugins (mini.nvim, lspconfig, treesitter, conform) so mini.hues colorscheme works based on WHEREAMI.

## Changes

- `lib/home/cook.mk` - override generic nvim zip rule to use bundle instead of raw staged binary; remove separate home-release target
- `.config/nvim/plugin/terminal.tl` - disable OSC passthrough which causes terminal blocking in server mode
