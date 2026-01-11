modules += lib
lib_lua_modules :=
lib_dirs :=
lib_libs :=
lib_srcs := lib/file.tl lib/platform.tl lib/ulid.tl lib/utils.tl lib/version.lua
lib_tests := lib/test_version.lua

# type declaration files for teal compilation
types_files := $(wildcard lib/types/*.d.tl lib/types/*/*.d.tl lib/types/*/*/*.d.tl)

# standalone lib files
lib_dirs += o/any/lib
lib_libs += o/any/lib/version.lua o/any/lib/platform.lua o/any/lib/utils.lua o/any/lib/ulid.lua o/any/lib/file.lua

o/any/lib/%.lua: lib/%.lua
	mkdir -p $(@D)
	cp $< $@

# compile .tl files to .lua (for o/any/lib, used by standalone modules)
# tl_staged must be regular prereq (not order-only) for parallel builds
o/any/lib/%.lua: lib/%.tl $(types_files) $(tl_staged)
	mkdir -p $(@D)
	$(tl_gen) -o $@ $<

# compile .tl files to .lua (for o/teal/lib via tl gen -o)
# uses secondary expansion so $(tl_staged) is evaluated after all includes
.SECONDEXPANSION:
o/teal/lib/%.lua: lib/%.tl $(types_files) $$(tl_staged)
	@mkdir -p $(@D)
	@$(tl_staged)/tl -- gen -o $@ $<

include lib/aerosnap/cook.mk
include lib/build/cook.mk
include lib/checker/cook.mk
include lib/claude/cook.mk
include lib/cosmic/cook.mk
include lib/daemonize/cook.mk
include lib/environ/cook.mk
include lib/home/cook.mk
include lib/nvim/cook.mk
include lib/skill/cook.mk
include lib/test/cook.mk
include lib/whereami/cook.mk
include lib/work/cook.mk
