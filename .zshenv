typeset -aU path

path=(
  "$HOME/.local/bin"
  "$HOME/.local/bootstrap/bin"
  "/Applications/Ghostty.app/Contents/MacOS"
  "/Applications/Hammerspoon.app/Contents/Frameworks/hs"
  "/Library/Application Support/org.pqrs/Karabiner-Elements/bin"
  "/opt/homebrew/bin"
  $path
)
path+=(~/.local/share/*/*-*(NOm/))
path+=(~/.local/share/*/*-*/bin(NOm/))
path+=(/usr/*/bin(N))

typeset -T LUA_PATH lua_path ';'
typeset -aU lua_path

lua_path=(
  "$HOME/extras/lua/?.lua"
  "$HOME/extras/lua/?/init.lua"
  "$HOME/extras/lua/3p/?.lua"
  "$HOME/lib/?.lua"
  "$HOME/lib/?/init.lua"
  "$HOME/lib/3p/?.lua"
)

LUA_PATH="${LUA_PATH};;"
export LUA_PATH

typeset -T LUA_CPATH lua_cpath ';'
typeset -aU lua_cpath

lua_cpath=(
  "$HOME/lib/?.so"
)

LUA_CPATH="${LUA_CPATH};;"
export LUA_CPATH

[ -r ~/.zprofile ] && source ~/.zprofile

export EDITOR=~/.local/bin/editor
export WHEREAMI=$(whereami)
export RIPGREP_CONFIG_PATH=~/.config/ripgrep/rg.conf
export COLORTERM=truecolor

export GIT_AUTHOR_EMAIL="189851+whilp@users.noreply.github.com"
export GIT_COMMITTER_EMAIL="189851+whilp@users.noreply.github.com"

# Install ghostty terminfo if missing or outdated
if [[ ! -f ~/.terminfo/x/xterm-ghostty ]] || [[ ~/.config/ghostty/term.tic -nt ~/.terminfo/x/xterm-ghostty ]]; then
  mkdir -p ~/.terminfo
  tic -x ~/.config/ghostty/term.tic 2>/dev/null
fi
export TERM=xterm
[ -r ~/extras/zshenv ] && source ~/extras/zshenv
