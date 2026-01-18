# spawn: fix fd passthrough when fd equals target

When passing `stdout = 1` or `stderr = 2` to spawn(), the code would:
1. Close the target fd (e.g., `close(1)`)
2. Try to dup the same fd (e.g., `dup(1)`)

This fails because the fd was just closed. The fix skips close/dup when the fd is already the correct target.

## Changes

- `lib/cosmic/spawn.tl` - Skip close/dup when opts.stdin == 0, opts.stdout == 1, or opts.stderr == 2
