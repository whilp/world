# home: simplify to platform-specific builds only

Remove the universal home + platform download mechanism. Each platform now has a self-contained `home-<platform>` binary with all tools included.

Previously, releases included a "universal" `home` binary (actually linux-x86_64) that would download platform-specific assets at runtime via `--with-platform`. This caused issues where symlinks pointed to the wrong platform's binaries.

The new approach is simpler: just ship platform-specific binaries directly.

## Changes

- `Makefile` - simplified release target, no more plain `home` artifact
- `.github/workflows/release.yml` - removed home-universal build/upload
- `lib/home/main.tl` - removed all platform detection/download logic and related options
- `lib/home/cook.mk` - removed gen-platforms from compilation, simplified test-release
- `lib/home/gen-platforms.tl` - deleted (no longer needed)
- `lib/home/test_release.tl` - deleted (tested removed mechanism)
- `lib/home/test_main.tl` - removed detect_platform test
