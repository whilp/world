lib_lua_modules :=
lib_dirs :=
lib_libs :=
lib_tests :=
luacheck_files :=

# standalone lib files
lib_dirs += o/any/lib
lib_libs += o/any/lib/version.lua o/any/lib/platform.lua o/any/lib/utils.lua o/any/lib/ulid.lua o/any/lib/file.lua

o/any/lib/%.lua: lib/%.lua
	mkdir -p $(@D)
	cp $< $@

include lib/aerosnap/cook.mk
include lib/build/cook.mk
include lib/claude/cook.mk
include lib/daemonize/cook.mk
include lib/environ/cook.mk
include lib/spawn/cook.mk
include lib/home/cook.mk
include lib/nvim/cook.mk
include lib/whereami/cook.mk
include lib/work/cook.mk

lib-test: $(luaunit) $(lib_tests) ## Run lib module tests

# luacheck runner
luacheck_script := lib/build/luacheck.lua
luacheck_bin = o/$(current_platform)/luacheck/bin/luacheck
luacheck_runner = $(lua_bin) $(luacheck_script)

lib-luacheck: $(luacheck_files) ## Run luacheck on lib modules

.PHONY: lib-test lib-luacheck
