# bootstrap: add missing .args file

The bootstrap binary was hanging because it lacked a `.args` file to tell
cosmopolitan lua which script to run. The cosmos lua base binary includes an
empty `tool/lua/.args`, and without an override the binary would wait for input
instead of running the bundled script.

## Changes

- `lib/home/cook.mk` - add `.args` file with `/zip/bootstrap.lua` to bootstrap build

## Validated

Tested on sprites.dev sprite (`world-bootstrap`):
- Bootstrap downloads and unpacks successfully
- nvim, gh, and other tools work
