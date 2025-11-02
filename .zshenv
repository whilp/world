[ -r ~/.zprofile ] && source ~/.zprofile

export RIPGREP_CONFIG_PATH=~/.config/ripgrep/rg.conf
export COLORTERM=truecolor

# Install ghostty terminfo if missing or outdated
if [[ ! -f ~/.terminfo/x/xterm-ghostty ]] || [[ ~/.config/ghostty/term.tic -nt ~/.terminfo/x/xterm-ghostty ]]; then
  mkdir -p ~/.terminfo
  tic -x ~/.config/ghostty/term.tic 2>/dev/null
fi
export TERM=xterm-ghostty

typeset -aU path

path=(
  "$HOME/.local/bin"
  "$HOME/.local/share/shimlink/bin"
  "$HOME/stripe/space-commander/bin"
  "/pay/deploy/claude-wrapper-hosts/current/bin"
  "/deploy/dev-tools/current/bin"
  "/opt/homebrew/bin"
  $path
)
path+=(/usr/*/bin(N))

typeset -T LUA_PATH lua_path ';'
typeset -aU lua_path

lua_path=(
  "$HOME/.local/lib/lua/?.lua"
  "$HOME/.local/lib/lua/3p/?.lua"
  "$HOME/.local/share/luajit-2.1/?.lua"
  ""
)

export LUA_PATH

[ -r ~/extras/zshenv ] && source ~/extras/zshenv
