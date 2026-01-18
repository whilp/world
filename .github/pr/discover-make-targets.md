# skill/hook: fix make help in SessionStart hook

Fix the SessionStart hook to properly display make targets by bootstrapping first.

- Makefile - add `bootstrap` target for explicit bootstrapping
- lib/skill/hook.lua - run `make bootstrap` before `make help`, use `bin/make` for portability

## Problem

The `session_start_make_help` handler runs `make help` on startup, but this fails on fresh checkouts because `o/bootstrap/cosmic` doesn't exist yet. The error was silently swallowed.

## Solution

1. Add explicit `bootstrap` target to Makefile (avoids hardcoding `o/` path)
2. Run `make bootstrap` before `make help` in the hook
3. Use `bin/make` wrapper to ensure cosmo-make is available regardless of PATH state

## Validation

- [x] hook outputs make targets on SessionStart
- [x] works on fresh checkout without existing build artifacts
