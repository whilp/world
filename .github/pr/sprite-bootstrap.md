# home: add bootstrap binary

Add a lightweight cosmopolitan binary that bootstraps a new environment by
downloading and unpacking the home bundle from whilp/world releases.

## Changes

- `lib/home/bootstrap.tl` - bootstrap source: detects platform, downloads home-<platform>, verifies SHA256, runs unpack
- `lib/home/test_bootstrap.tl` - tests for platform detection
- `lib/home/cook.mk` - build rule for bootstrap binary
- `.github/workflows/release.yml` - upload bootstrap artifact
- `Makefile` - include bootstrap in release assets

## Usage

The bootstrap binary is meant to be curled and executed on a fresh machine:

```bash
curl -fsSL <bootstrap-url> -o /tmp/bootstrap && chmod +x /tmp/bootstrap && /tmp/bootstrap
```

It will:
1. Detect platform (linux-x86_64, linux-arm64, darwin-arm64)
2. Fetch latest release from whilp/world
3. Download home-<platform> and SHA256SUMS
4. Verify checksum
5. Run `home unpack --force ~/`
