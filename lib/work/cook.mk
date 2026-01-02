lib_lua_modules += work
lib_dirs += o/any/work/lib

work_src := $(filter-out lib/work/test%.lua,$(wildcard lib/work/*.lua))
work_lib := $(patsubst lib/%,o/any/work/lib/%,$(work_src))
lib_libs += $(work_lib)

o/any/work/lib/work/%.lua: lib/work/%.lua
	mkdir -p $(@D)
	cp $< $@
