# cosmic: pass script args as varargs to match standard lua

Fixes cosmic to pass script arguments as varargs to script chunks, matching standard Lua behavior. Previously cosmic used dofile() which only set up the global arg table but didn't pass varargs, breaking scripts that use the common `function main(...) end` pattern.

## Changes

- lib/cosmic/main.lua - change from dofile() to loadfile() + chunk() with unpacked args
  - Replaces dofile(opts.script) with loadfile() to get the chunk
  - Calls chunk with table.unpack(opts.script_args, 1, #opts.script_args) to pass arg[1], arg[2], ... as varargs
  - Matches standard lua behavior where scripts receive both global arg table AND varargs
  - Note: table.unpack has ~250 arg limit but this is not a practical concern for CLI scripts
  - Preserves all existing functionality (--skill, -e, -l, etc.)

- lib/cosmic/test_args.lua - comprehensive test suite for argument passing
  - Tests varargs-only scripts using (...)
  - Tests arg table-only scripts using global arg
  - Tests combined usage (both varargs and arg table should match)
  - Tests main(...) function pattern used by reporter scripts
  - Tests edge cases: no args, spaces in args, special characters
  - Tests compatibility with cosmo.is_main() guard pattern

## Validation

- [x] comprehensive test suite added with 10 test cases
- [x] verified main(...) pattern works (used by 7 reporter scripts)
- [x] verified both varargs and arg table work correctly
- [x] verified --skill pr still works
- [x] no changes needed to existing reporter scripts (they use main(...) which now works)
