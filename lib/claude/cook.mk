lib_lua_modules += claude
lib_dirs += o/any/claude/lib
lib_libs += o/any/claude/lib/claude/main.lua
lib_tests += o/any/claude/test.ok

o/any/claude/lib/claude/main.lua: lib/claude/main.lua
	mkdir -p $(@D)
	cp $< $@

o/any/claude/test.ok: lib/claude/test.lua o/any/claude/lib/claude/main.lua $(runner)
	$(runner) $< $@
