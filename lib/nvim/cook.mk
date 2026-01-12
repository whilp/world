modules += nvim-lib
nvim-lib_tl_srcs := $(wildcard lib/nvim/*.tl)
nvim-lib_lua_srcs := $(wildcard lib/nvim/*.lua)
nvim-lib_srcs := $(nvim-lib_tl_srcs) $(nvim-lib_lua_srcs)
nvim-lib_tests := $(filter lib/nvim/test%.tl,$(nvim-lib_tl_srcs))
nvim-lib_files := $(patsubst lib/%.tl,o/any/lib/%.lua,$(filter-out %test.tl,$(nvim-lib_tl_srcs)))
nvim-lib_deps := cosmic daemonize whereami

lib_lua_modules += nvim
