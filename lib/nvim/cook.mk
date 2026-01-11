modules += nvim-lib
nvim-lib_tl_srcs := $(wildcard lib/nvim/*.tl)
nvim-lib_lua_srcs := $(wildcard lib/nvim/*.lua)
nvim-lib_srcs := $(nvim-lib_tl_srcs) $(nvim-lib_lua_srcs)
nvim-lib_tests := $(filter lib/nvim/test%.lua,$(nvim-lib_lua_srcs))
nvim-lib_files := o/any/nvim/lib/nvim/main.lua

lib_lua_modules += nvim
lib_dirs += o/any/nvim/lib

o/any/nvim/lib/nvim/main.lua: $(o)/lib/nvim/main.lua
	mkdir -p $(@D)
	cp $< $@
