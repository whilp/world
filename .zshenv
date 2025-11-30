typeset -aU path

path=(
  "$HOME/.local/bin"
  "$HOME/.local/bootstrap/bin"
  "$HOME/.local/share/shimlink/bin"
  "/Applications/Ghostty.app/Contents/MacOS"
  "/Applications/Hammerspoon.app/Contents/Frameworks/hs"
  "/Library/Application Support/org.pqrs/Karabiner-Elements/bin"
  "/opt/homebrew/bin"
  $path
)
path+=(/usr/*/bin(N))

typeset -T LUA_PATH lua_path ';'
typeset -aU lua_path

lua_path=(
  "$HOME/extras/lua/?.lua"
  "$HOME/extras/lua/3p/?.lua"
  "$HOME/.local/lib/lua/?.lua"
  "$HOME/.local/lib/lua/3p/?.lua"
  "$HOME/.local/bootstrap/share/lua/5.1/?.lua"
  "$HOME/.local/bootstrap/share/lua/5.1/?/init.lua"
  "$HOME/.local/share/luajit-2.1/?.lua"
  "$HOME/.local/share/luajit-2.1/../../share/lua/5.1/?.lua"
  "$HOME/.local/share/luajit-2.1/../../share/lua/5.1/?/init.lua"
)

# append ;; to preserve built-in paths for bundled rocks in relocated luajit installs
LUA_PATH="${LUA_PATH};;"
export LUA_PATH

typeset -T LUA_CPATH lua_cpath ';'
typeset -aU lua_cpath

lua_cpath=(
  "$HOME/.local/bootstrap/lib/lua/5.1/?.so"
  "$HOME/.local/bootstrap/lib/lua/5.1/?/?.so"
  "$HOME/.local/share/luajit-2.1/../../lib/lua/5.1/?.so"
  "$HOME/.local/share/luajit-2.1/../../lib/lua/5.1/?/?.so"
)

LUA_CPATH="${LUA_CPATH};;"
export LUA_CPATH

[ -r ~/.zprofile ] && source ~/.zprofile

export WHEREAMI=$(whereami)
export RIPGREP_CONFIG_PATH=~/.config/ripgrep/rg.conf
export COLORTERM=truecolor

# Install ghostty terminfo if missing or outdated
if [[ ! -f ~/.terminfo/x/xterm-ghostty ]] || [[ ~/.config/ghostty/term.tic -nt ~/.terminfo/x/xterm-ghostty ]]; then
  mkdir -p ~/.terminfo
  tic -x ~/.config/ghostty/term.tic 2>/dev/null
fi
export TERM=xterm
[ -r ~/extras/zshenv ] && source ~/extras/zshenv
