# box: add git identity configuration to bootstrap

Copies git user.name and user.email during box bootstrap, alongside the existing GitHub token setup.

## Changes

- `lib/box/backend.tl` - add `Git` record type with name/email fields to `Env`
- `lib/box/run.tl` - add `configure_git` function that sets global git config
