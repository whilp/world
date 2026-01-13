modules += gas
gas_tl_srcs := $(wildcard lib/gas/*.tl)
gas_srcs := $(gas_tl_srcs)
gas_tests := $(filter lib/gas/test_%.tl,$(gas_tl_srcs))
gas_tl_libs := $(patsubst %.tl,$(o)/%.lua,$(gas_tl_srcs))
gas_libs := $(gas_tl_libs)
gas_files := $(gas_libs)
lib_lua_modules += gas
