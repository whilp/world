[ -r ~/.zprofile ] && source ~/.zprofile

# Install ghostty terminfo if missing or outdated
if [[ ! -f ~/.terminfo/x/xterm-ghostty ]] || [[ ~/.config/ghostty/term.tic -nt ~/.terminfo/x/xterm-ghostty ]]; then
  mkdir -p ~/.terminfo
  tic -x ~/.config/ghostty/term.tic 2>/dev/null
fi
export TERM=xterm-ghostty

# LuaJIT module path (symlink managed by shimlink)
export LUA_PATH="$HOME/.local/share/luajit-2.1/?.lua;;"
