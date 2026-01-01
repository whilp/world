lib_lua_modules += work
lib_dirs += o/any/work/lib

work_src := $(filter-out lib/work/test%.lua,$(wildcard lib/work/*.lua))
work_lib := $(patsubst lib/%,o/any/work/lib/%,$(work_src))
work_test_files := $(filter-out lib/work/test_lib.lua,$(wildcard lib/work/test*.lua))
work_tests := $(patsubst lib/work/test%.lua,o/any/work/test%.ok,$(work_test_files))
lib_libs += $(work_lib)
lib_tests += $(work_tests)

o/any/work/lib/work/%.lua: lib/work/%.lua
	mkdir -p $(@D)
	cp $< $@

o/any/work/test.ok: lib/work/test.lua $(work_lib) $(runner)
	$(runner) $< $@

o/any/work/test_%.ok: lib/work/test_%.lua $(work_lib) $(runner)
	$(runner) $< $@
