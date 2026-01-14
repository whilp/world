# home: use unzip for extraction, bundle symlinks

The home binary extraction process used custom Lua code that couldn't preserve symlinks. This change refactors extraction to use the bundled `unzip` binary, which properly handles symlinks and file permissions.

## Key changes

1. **Build time:** Create `dotfiles.zip` with `-y` flag to preserve symlinks, including `lua` -> `cosmic-lua`
2. **Runtime extraction:** Use bundled `unzip` binary instead of custom `copy_file()` logic
3. **Simplified code:** Removed ~60 lines of custom extraction logic in favor of standard `unzip`

## Benefits

- ✅ Symlinks preserved in dotfiles (including `lua` -> `cosmic-lua`)
- ✅ File permissions preserved by zip format
- ✅ Simpler, more maintainable code
- ✅ Leverages battle-tested `unzip` instead of custom implementation

## Files changed

- `lib/home/cook.mk` - create dotfiles.zip with symlinks, bundle as whole file
- `lib/home/main.tl` - use unzip for extraction, fallback to manifest for tests
- `lib/home/test_main.tl` - update tests to provide zip_root for validation tests
