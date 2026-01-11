# cosmic: bundle teal compiler for bootstrap

Bundle the teal compiler (tl.lua) and a teal transpiler script (tl-gen.lua) into the cosmic binary to enable self-hosted teal compilation.

- lib/cosmic/cook.mk - add tl dependency and bundle tl.lua with tl-gen.lua
- lib/cosmic/tl-gen.lua - new script that uses bundled teal for transpilation

## Implementation

The tl-gen.lua script uses teal's low-level API (lex + parse_program + generate) to transpile .tl files to .lua without type checking. This is sufficient for bootstrap purposes.

Usage:
```bash
cosmic -- /zip/tl-gen.lua input.tl -o output.lua
```

## Impact

Once this version of cosmic is released, we can remove the lib/build/*.lua bootstrap files that were needed during the teal migration, since cosmic will be able to compile its own .tl source files.

## Validation

- [x] make clean test passes
- [x] teal transpilation works correctly
- [x] cosmic binary includes tl.lua and tl-gen.lua
