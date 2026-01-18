# nvim: hide cmdline when not in use

Adds autocommands to dynamically show/hide the command line based on usage.

## Changes

- `.config/nvim/plugin/interface.tl` - set cmdheight=0 by default, show when entering ":" command mode, hide on leave or empty input
