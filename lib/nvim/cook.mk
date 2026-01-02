lib_lua_modules += nvim
lib_dirs += o/any/nvim/lib
lib_libs += o/any/nvim/lib/nvim/main.lua

o/any/nvim/lib/nvim/main.lua: lib/nvim/main.lua
	mkdir -p $(@D)
	cp $< $@
