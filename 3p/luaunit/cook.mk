luaunit_url := https://raw.githubusercontent.com/bluebird75/luaunit/LUAUNIT_V3_4/luaunit.lua
luaunit_sha256 := bf3e3fb25b77739fa1ebc324582776d26486e32e49c150628bc21b9b9e6ce645
luaunit_dir := $(3p)/luaunit
luaunit_file := $(luaunit_dir)/luaunit.lua
luaunit_lua_dir := $(luaunit_dir)/.lua

$(luaunit_file): | $(luaunit_dir)
	$(curl) -o $@ $(luaunit_url)
	cd $(dir $@) && echo "$(luaunit_sha256)  $(notdir $@)" | $(sha256sum) -c

$(luaunit_dir):
	mkdir -p $@

$(luaunit_lua_dir)/luaunit.lua: $(luaunit_file)
	mkdir -p $(dir $@)
	cp $(luaunit_file) $@

luaunit_libs := $(luaunit_lua_dir)
