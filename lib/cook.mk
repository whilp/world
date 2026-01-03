lib_lua_modules :=
lib_dirs :=
lib_libs :=
lib_tests :=

# standalone lib files
lib_dirs += o/any/lib
lib_libs += o/any/lib/version.lua o/any/lib/platform.lua o/any/lib/utils.lua o/any/lib/ulid.lua o/any/lib/file.lua

o/any/lib/%.lua: lib/%.lua
	mkdir -p $(@D)
	cp $< $@

include lib/aerosnap/cook.mk
include lib/build/cook.mk
include lib/claude/cook.mk
include lib/cosmic/cook.mk
include lib/daemonize/cook.mk
include lib/environ/cook.mk
include lib/home/cook.mk
include lib/nvim/cook.mk
include lib/skill/cook.mk
include lib/whereami/cook.mk
include lib/work/cook.mk
