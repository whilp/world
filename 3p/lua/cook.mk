# lua binary from whilp/cosmopolitan fork (via cosmos)
# pre-built with all C extensions (unix, path, re, sqlite3, argon2, json, cosmo)
# we just need to zip in pure lua modules (luaunit, luacheck)

lua_bin := results/bin/lua
lua_ape := results/bin/lua.ape

# minimal lua for testing - only luaunit bundled
lua_test := o/bin/lua-test

results/bin:
	mkdir -p $@

o/bin:
	mkdir -p $@

lib_lua_files := $(filter-out lib/test_%.lua,$(wildcard lib/*.lua))
lib_lua_dir := o/3p/lib/.lua
lib_lua_stamp := $(lib_lua_dir)/.copied

$(lib_lua_stamp): $(lib_lua_files) $(wildcard lib/spawn/*.lua)
	mkdir -p $(lib_lua_dir)
	cp $(lib_lua_files) $(lib_lua_dir)
	cp -r lib/spawn $(lib_lua_dir)
	touch $@

# test binary: cosmos lua + luaunit only
$(lua_test): private .UNVEIL = rx:$(cosmos_lua_bin) r:$(luaunit_lua_dir) rx:$(cosmos_zip_bin) rwc:o/bin rw:/dev/null
$(lua_test): private .PLEDGE = stdio rpath wpath cpath fattr exec proc
$(lua_test): $(cosmos_lua_bin) $(cosmos_zip_bin) $(luaunit_lua_dir)/luaunit.lua | o/bin
	cp $(cosmos_lua_bin) $@
	chmod +x $@
	cd $(luaunit_lua_dir)/.. && $(cosmos_zip_bin) -qr $(CURDIR)/$@ $(notdir $(luaunit_lua_dir))
	./$@ --assimilate || true

# full binary: cosmos lua + luaunit + luacheck + lib modules
$(lua_ape): private .UNVEIL = rx:$(cosmos_lua_bin) r:$(luaunit_lua_dir) r:$(luacheck_lua_dir) r:lib r:o/3p/lib rx:$(cosmos_zip_bin) rwc:results/bin rwc:o/3p/lib rw:/dev/null
$(lua_ape): private .PLEDGE = stdio rpath wpath cpath fattr exec proc
$(lua_ape): $(cosmos_lua_bin) $(cosmos_zip_bin) $(luaunit_lua_dir)/luaunit.lua $(luacheck_lua_dir)/bin/luacheck $(lib_lua_stamp) | results/bin
	cp $(cosmos_lua_bin) $@
	chmod +x $@
	cd $(luaunit_lua_dir)/.. && $(cosmos_zip_bin) -qr $(CURDIR)/$@ $(notdir $(luaunit_lua_dir))
	cd $(luacheck_lua_dir)/.. && $(cosmos_zip_bin) -qr $(CURDIR)/$@ $(notdir $(luacheck_lua_dir))
	cd o/3p/lib && $(cosmos_zip_bin) -qr $(CURDIR)/$@ .lua

$(lua_bin): $(lua_ape)
	cp $< $@
	./$@ --assimilate || true

lua: $(lua_bin) ## Build lua with bundled modules

lua-skill: $(lua_bin) ## Generate cosmo-lua skill
	rm -rf .claude/skills/cosmo-lua
	$(lua_bin) --skill .

clean-lua:
	rm -rf $(lua_bin) $(lua_ape) $(lua_test) o/3p/lib

.PHONY: lua lua-skill clean-lua
