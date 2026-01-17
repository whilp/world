# bootstrap: fix macOS platform detection

`cosmo.GetHostOs()` returns `XNU` (the kernel name) on macOS, not `OSX`.
This caused bootstrap to fail on macOS with "unexpected platform: xnu-arm64".

## Changes

- `lib/home/bootstrap.tl` - handle both `xnu` and `osx` when normalizing to `darwin`
