modules += lib
lib_lua_modules :=
lib_dirs :=
lib_libs :=
lib_tests := lib/test_version.tl

# type declaration files for teal compilation
types_files := $(wildcard lib/types/*.d.tl lib/types/*/*.d.tl lib/types/*/*/*.d.tl)

# standalone lib files (use _tl_files mechanism)
lib_dirs += o/lib
lib_tl_files := lib/platform.tl lib/ulid.tl lib/utils.tl
lib_libs += o/lib/version.lua

# copy .lua files to o/lib/
o/lib/%.lua: lib/%.lua
	@mkdir -p $(@D)
	@cp $< $@

# compile .tl files to .lua (for o/teal/lib)
# TODO: replace with cosmic --compile when it supports transpile-only mode
o/teal/lib/%.lua: lib/%.tl $(types_files) | $(bootstrap_files)
	@mkdir -p $(@D)
	@$(bootstrap_cosmic) /zip/tl-gen.lua $< -o $@

include lib/aerosnap/cook.mk
include lib/appscript/cook.mk
include lib/box/cook.mk
include lib/build/cook.mk
include lib/checker/cook.mk
include lib/claude/cook.mk
include lib/daemonize/cook.mk
include lib/environ/cook.mk
include lib/home/cook.mk
include lib/nvim/cook.mk
include lib/skill/cook.mk
include lib/test/cook.mk
include lib/whereami/cook.mk

# After includes: derive lib_libs from lib module _tl_files
lib_libs += $(patsubst %.tl,$(o)/%.lua,$(foreach m,$(lib_lua_modules),$($(m)_tl_files)))
