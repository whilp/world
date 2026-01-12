modules += aerosnap
aerosnap_srcs := $(wildcard lib/aerosnap/*.tl)
aerosnap_tests := lib/aerosnap/test.tl
aerosnap_files := o/any/aerosnap/lib/aerosnap/init.lua
lib_lua_modules += aerosnap
lib_dirs += o/any/aerosnap/lib
lib_libs += o/any/aerosnap/lib/aerosnap/init.lua

# uses secondary expansion so $(tl_files) is evaluated after 3p/tl/cook.mk
.SECONDEXPANSION:
o/any/aerosnap/lib/aerosnap/init.lua: lib/aerosnap/init.tl $(types_files) $$(tl_files) | $$(tl_staged)
	@mkdir -p $(@D)
	@$(tl_gen) -o $@ $<

# test depends on module being compiled
$(o)/lib/aerosnap/test.tl.test.ok: o/any/aerosnap/lib/aerosnap/init.lua
