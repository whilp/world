# ghostty: unbind cmd+shift+{h,j,k,l} for nvim splits

Unbind cmd+shift+{h,j,k,l} in Ghostty so these keys pass through to nvim for window splitting.

## Changes

- `.config/ghostty/config` - add unbinds for cmd+shift+h/j/k/l to allow nvim `<D-S-*>` split keybindings to work
