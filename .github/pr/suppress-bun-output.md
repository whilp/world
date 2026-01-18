# clasp: suppress bun output in CI

Redirect stdout from `bun install` and `bun build --compile` to `/dev/null` to keep CI logs clean.

## Changes

- `3p/clasp/cook.mk` - add `>/dev/null` to bun install and bun build commands
