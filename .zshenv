[ -r ~/.zprofile ] && source ~/.zprofile

# Install ghostty terminfo if missing or outdated
if [[ ! -f ~/.terminfo/x/xterm-ghostty ]] || [[ ~/.config/ghostty/term.tic -nt ~/.terminfo/x/xterm-ghostty ]]; then
  mkdir -p ~/.terminfo
  tic -x ~/.config/ghostty/term.tic 2>/dev/null
fi
export TERM=xterm-ghostty

# Lua module paths
# - luajit-2.1: symlink managed by shimlink for LuaJIT standard library
# - lua: personal Lua libraries
# - lua/3p: vendored third-party libraries
export LUA_PATH="$HOME/.local/share/lua/?.lua;$HOME/.local/share/lua/3p/?.lua;$HOME/.local/share/luajit-2.1/?.lua;;"
