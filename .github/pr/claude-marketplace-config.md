# box: add marketplace config to seeded claude.json

Add marketplace auto-install flags to the default claude.json config so boxes don't attempt to auto-install marketplace extensions.

## Changes

- `lib/box/claude.tl` - add officialMarketplaceAutoInstallAttempted and officialMarketplaceAutoInstalled flags to seeded config
