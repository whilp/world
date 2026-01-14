# home: compile setup and mac modules at build time

Fixes module loading error where setup.sh failed with "module 'setup.setup' not found". The issue occurred because Teal source files were being bundled into the home binary without compilation, but the runtime only has a Lua interpreter.

## Changes

- `lib/home/cook.mk` - Declare `home_tl_files` to include setup and mac modules, rely on Makefile's existing pattern rules for compilation, copy compiled .lua files instead of source .tl files
