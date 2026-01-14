# home: compile setup and mac modules at build time

Fixes module loading error where setup.sh failed with "module 'setup.setup' not found". The issue occurred because Teal source files were being bundled into the home binary without compilation, but the runtime only has a Lua interpreter.

## Changes

- `lib/home/cook.mk` - Add compilation rules for setup and mac Teal modules, update home binary build to use compiled .lua files instead of source .tl files
