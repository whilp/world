modules += checker
checker_lua_srcs := $(wildcard lib/checker/*.lua)
checker_tl_srcs := $(wildcard lib/checker/*.tl)
checker_srcs := $(checker_lua_srcs) $(checker_tl_srcs)
checker_tests := $(filter lib/checker/test_%.lua,$(checker_lua_srcs))
# output files: .lua sources copied, .tl sources compiled to .lua
checker_lua_files := $(addprefix $(o)/,$(filter-out $(checker_tests),$(checker_lua_srcs)))
checker_tl_files := $(patsubst %.tl,$(o)/%.lua,$(checker_tl_srcs))
checker_files := $(checker_lua_files) $(checker_tl_files)
