modules += whereami
whereami_srcs := lib/whereami/init.tl lib/whereami/test.tl
whereami_tests := lib/whereami/test.tl
whereami_files := o/any/whereami/lib/whereami/init.lua

lib_lua_modules += whereami
lib_dirs += o/any/whereami/lib

# Use secondary expansion for tl_files (defined in 3p/tl/cook.mk after this file)
.SECONDEXPANSION:
o/any/whereami/lib/whereami/init.lua: lib/whereami/init.tl $(types_files) $$(tl_files) | $$(tl_staged)
	@mkdir -p $(@D)
	@$(tl_gen) -o $@ $<
