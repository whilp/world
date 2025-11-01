[ -r ~/.zprofile ] && source ~/.zprofile

# Install ghostty terminfo if missing or outdated
if [[ ! -f ~/.terminfo/x/xterm-ghostty ]] || [[ ~/.config/ghostty/term.tic -nt ~/.terminfo/x/xterm-ghostty ]]; then
  mkdir -p ~/.terminfo
  tic -x ~/.config/ghostty/term.tic 2>/dev/null
fi
export TERM=xterm-ghostty

typeset -T LUA_PATH lua_path ';'
typeset -aU lua_path

lua_path=(
  "$HOME/.local/lib/lua/?.lua"
  "$HOME/.local/lib/lua/3p/?.lua"
  "$HOME/.local/share/luajit-2.1/?.lua"
  ""
)

[ -r ~/extras/zshenv ] && source ~/extras/zshenv
