modules += environ
environ_srcs := $(wildcard lib/environ/*.tl)
environ_tests := lib/environ/test.tl
lib_lua_modules += environ
lib_dirs += o/any/environ/lib
lib_libs += o/any/environ/lib/environ/init.lua

o/any/environ/lib/environ/init.lua: lib/environ/init.tl $(types_files) | $(tl_staged)
	mkdir -p $(@D)
	$(tl_gen) -o $@ $<

# test depends on module being compiled
$(o)/lib/environ/test.tl.test.ok: o/any/environ/lib/environ/init.lua
