# home: remove --with-platform flag support

Remove support for the deprecated `--with-platform` flag in the home binary and update setup.sh to not use it.

## Changes

- `lib/home/main.tl` - refactor parse_args to use cosmo.getopt instead of manual parsing, which properly rejects unknown flags
- `lib/home/test_main.tl` - add tests to verify unknown flags are rejected
- `setup.sh` - remove --with-platform flag from home unpack command

## Background

The `--with-platform` flag was previously used to download platform-specific assets at runtime. This is no longer needed since we now build platform-specific binaries directly. The flag was still being passed in setup.sh but was silently ignored due to manual arg parsing that didn't validate unknown flags.

By switching to cosmo.getopt, we now properly reject unknown flags and prevent this kind of issue in the future.
