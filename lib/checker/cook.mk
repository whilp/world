modules += checker
checker_lua_srcs := $(wildcard lib/checker/*.lua)
checker_tl_srcs := $(wildcard lib/checker/*.tl)
checker_srcs := $(checker_lua_srcs) $(checker_tl_srcs)
checker_tests := $(filter lib/checker/test_%.tl,$(checker_tl_srcs))
# output files: .lua sources copied to o/any/, .tl sources compiled via tlconfig.lua build_dir
checker_lua_files := $(addprefix o/any/,$(filter-out $(checker_tests),$(checker_lua_srcs)))
checker_tl_files := $(patsubst lib/%.tl,o/teal/lib/%.lua,$(checker_tl_srcs))
checker_files := $(checker_lua_files) $(checker_tl_files)
