# build: add missing bootstrap_cosmic dependencies

Fix release workflow failure caused by missing dependencies on `$(bootstrap_cosmic)`.

## Problem

With parallel builds (`-j`), rules that use `$(bootstrap_cosmic)` in their recipe can start before the binary is built, causing:

```
/bin/bash: line 1: o/bootstrap/cosmic: No such file or directory
```

## Changes

- `lib/build/cook.mk` - add order-only dependency to make-help.snap rule
- `Makefile` - add order-only dependency to help target
- `Makefile` - add order-only dependency to snap.test.ok pattern rule

## Validation

- [x] `make clean && make -j check test build` passes
- [x] all 39 tests pass
