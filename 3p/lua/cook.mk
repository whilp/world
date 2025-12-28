# lua binary from whilp/cosmopolitan fork (via cosmos)
# pre-built with all C extensions (unix, path, re, sqlite3, argon2, json, cosmo)
# we just need to zip in pure lua modules (luaunit, luacheck)

lua_bin := results/bin/lua
lua_ape := results/bin/lua.ape

results/bin:
	mkdir -p $@

lib_lua_files := $(filter-out lib/test_%.lua lib/run-test.lua,$(wildcard lib/*.lua))
lib_lua_dir := o/3p/lib/.lua
lib_lua_stamp := $(lib_lua_dir)/.copied

$(lib_lua_stamp): $(lib_lua_files) $(wildcard lib/spawn/*.lua)
	mkdir -p $(lib_lua_dir)
	cp $(lib_lua_files) $(lib_lua_dir)
	cp -r lib/spawn $(lib_lua_dir)
	touch $@

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

lua: $(lua_bin)

test-3p-lua: private .UNVEIL = r:3p/lua rx:$(lua_bin) r:$(test_runner) rwc:3p/lua/o rw:/dev/null
test-3p-lua: private .PLEDGE = stdio rpath wpath cpath proc exec
test-3p-lua: private .CPU = 60
test-3p-lua: lua
	cd 3p/lua && $(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test.lua

clean-lua:
	rm -rf $(lua_bin) $(lua_ape) o/3p/lib

.PHONY: lua clean-lua test-3p-lua
