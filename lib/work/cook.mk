modules += work
lib_lua_modules += work
work_srcs := $(wildcard lib/work/*.lua) $(wildcard lib/work/*.tl)
work_tests := $(filter-out lib/work/test_lib.tl,$(wildcard lib/work/test_*.tl))

work_lua_src := $(filter-out lib/work/test%.lua,$(wildcard lib/work/*.lua))
work_tl_src := $(filter-out lib/work/test%.tl,$(wildcard lib/work/*.tl))
work_lua_lib := $(patsubst lib/%,o/any/lib/%,$(work_lua_src))
work_tl_lib := $(patsubst lib/%.tl,o/any/lib/%.lua,$(work_tl_src))
lib_libs += $(work_lua_lib) $(work_tl_lib)
