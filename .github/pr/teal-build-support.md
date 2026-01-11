# build: add tl gen compilation support for .tl files

Add build system infrastructure for compiling Teal (.tl) files to Lua. This is PR 1.2 of the teal migration plan.

- 3p/tl/tl-gen.lua - wrapper script for tl gen (works around -o option bug)
- 3p/tl/cook.mk - add tl_gen command using wrapper script
- Makefile - pattern rule $(o)/%.lua: %.tl for compilation
- lib/cook.mk - types_files variable and pattern for standalone libs
- lib/checker/cook.mk - handle both .lua and .tl sources
- lib/cosmic/cook.mk - handle both .lua and .tl sources
- lib/skill/cook.mk - handle both .lua and .tl sources

When a .tl file is added to a module:
1. The *_tl_srcs variable picks it up via wildcard
2. The pattern rule compiles it using tl gen
3. The compiled .lua file goes to o/ alongside other lua files

## Validation

- [x] make clean test passes
- [x] make teal passes
- [x] test .tl file compiles correctly to o/
