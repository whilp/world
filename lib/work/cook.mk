modules += work
work_tl_files := $(filter-out lib/work/test%.tl,$(wildcard lib/work/*.tl))
work_tests := $(filter-out lib/work/test_lib.tl,$(wildcard lib/work/test_*.tl))
lib_lua_modules += work
