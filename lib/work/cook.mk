modules += work
lib_lua_modules += work
lib_dirs += o/any/work/lib
work_srcs := $(wildcard lib/work/*.lua) $(wildcard lib/work/*.tl)
work_tests := $(filter-out lib/work/test_lib.tl,$(wildcard lib/work/test_*.tl))

work_all_lua := $(wildcard lib/work/*.lua)
work_tl_srcs := $(wildcard lib/work/*.tl)
work_lua_src := $(filter-out lib/work/test%.lua,$(work_all_lua))
work_lua_lib := $(patsubst lib/%,o/any/work/lib/%,$(work_lua_src))
work_tl_lib := $(patsubst lib/%.tl,o/any/work/lib/%.lua,$(work_tl_srcs))
work_lib := $(work_lua_lib) $(work_tl_lib)
lib_libs += $(work_lib)

o/any/work/lib/work/%.lua: lib/work/%.lua
	mkdir -p $(@D)
	cp $< $@

# tl files: compile via o/teal/lib, then copy to o/any/work/lib
o/any/work/lib/work/%.lua: $(o)/teal/lib/work/%.lua
	mkdir -p $(@D)
	cp $< $@
