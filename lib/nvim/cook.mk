modules += nvim
nvim_tl_srcs := $(wildcard lib/nvim/*.tl)
nvim_lua_srcs := $(wildcard lib/nvim/*.lua)
nvim_srcs := $(nvim_tl_srcs) $(nvim_lua_srcs)
nvim_tests := $(filter lib/nvim/test%.lua,$(nvim_lua_srcs))
nvim_files := o/any/nvim/lib/nvim/main.lua

lib_lua_modules += nvim
lib_dirs += o/any/nvim/lib

o/any/nvim/lib/nvim/main.lua: $(o)/lib/nvim/main.lua
	mkdir -p $(@D)
	cp $< $@
