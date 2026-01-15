# argparse: allow unknown options to pass through in nvim

Fix nvim wrapper to allow unknown options like `--remote-ui` to pass through to the actual nvim binary. The previous conversion to `cosmo.getopt` (#369) caused the wrapper to error on unknown options, breaking common nvim invocations.

## Changes

- `lib/nvim/main.tl` - revert `parse_socket_option` to manual parsing that only extracts `--socket` and passes all other options through

The nvim wrapper is unique in that it only needs to extract its own `--socket` option while passing through all nvim options it doesn't understand. Unlike `cleanshot` and `tl-gen`, which control all their options and should reject unknown ones, nvim must be transparent to nvim's full option set.
