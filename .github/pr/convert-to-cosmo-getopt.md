# argparse: convert manual parsing to cosmo.getopt

Convert remaining manual argument parsing implementations to use cosmo.getopt for consistency and proper validation. This follows the pattern established in PR #367 which converted the home binary.

## Changes

- `lib/nvim/main.tl` - convert parse_socket_option to use getopt for --socket flag
- `lib/cleanshot/init.tl` - convert parse_flags to use getopt for all cleanshot flags
- `lib/cosmic/tl-gen.lua` - convert manual -o/--output parsing to use getopt

All three modules now properly validate options and reject unknown flags with error messages, improving consistency across the codebase.
