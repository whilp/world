lib_lua_modules += claude
lib_dirs += o/any/claude/lib
lib_libs += o/any/claude/lib/claude/main.lua

o/any/claude/lib/claude/main.lua: lib/claude/main.lua
	mkdir -p $(@D)
	cp $< $@
