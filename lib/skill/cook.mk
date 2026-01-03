# lib/skill - cosmic skill modules

skill_lib := $(o_any)/skill/lib

lib_lua_modules += skill
lib_dirs += $(skill_lib)
lib_libs += $(skill_lib)/skill/init.lua

$(skill_lib)/skill/%.lua: lib/skill/%.lua
	mkdir -p $(@D)
	cp $< $@

# skill module depends on build.pr
$(skill_lib)/skill/init.lua: $(o_any)/build/lib/build/pr.lua
