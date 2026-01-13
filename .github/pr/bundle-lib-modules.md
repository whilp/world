# home: bundle compiled lib modules for bin scripts

Enable Teal type checking for `.local/bin` scripts by bundling compiled lib modules into the home binary with versioned paths.

- lib/cleanshot/cook.mk - wire cleanshot into build system
- lib/cleanshot/init.tl - fix Flags type (record -> type alias)
- lib/cook.mk - include cleanshot module
- lib/home/cook.mk - add home_bin_lib_modules, bundle to versioned paths
- lib/home/main.tl - refactor symlink creation into reusable helper
- lib/home/test_main.tl - add tests for versioned paths and symlinks
- .zshenv - add .local/lib to lua_path array

## Structure after unpack

```
.local/bin/aerosnap                           # thin wrapper
.local/lib/aerosnap/20260112-abc123/init.lua  # compiled module
.local/lib/aerosnap/init.lua                  # symlink
```

The symlink allows `require("aerosnap")` to resolve via LUA_PATH pattern `$HOME/.local/lib/?/init.lua`.

## Validation

- [x] make test passes
- [x] versioned paths in home binary verified
- [x] symlink creation tested end-to-end
