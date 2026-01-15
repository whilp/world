# setup: fetch platform-specific home binaries

Updates setup.sh to detect the platform (OS and architecture) and fetch the appropriate platform-specific home binary, since releases no longer produce a plain "home" binary.

## Changes

- `setup.sh` - adds detect_platform() function to determine OS and architecture, updates install_home() to fetch home-{platform} binaries (home-darwin-arm64, home-linux-x86_64, or home-linux-arm64)
