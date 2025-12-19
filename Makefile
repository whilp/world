include 3p/cook.mk
include 3p/luaunit/cook.mk
include 3p/cosmopolitan/cook.mk
include 3p/lua/cook.mk

build: lua
test: lua
	$(lua_bin) 3p/lua/test.lua

clean:
	rm -rf o results

home_exclude_pattern = ^(3p/|o/|results/|Makefile|home/|\.git)

results/dotfiles.zip:
	git ls-files -z | grep -zZvE '$(home_exclude_pattern)' | \
		xargs -0 $(cosmos_zip_bin) -q $@

results/bin/home: $(lua_bin) results/dotfiles.zip home/main.lua home/.args
	cp $(lua_bin) $@
	$(cosmos_zip_bin) -qj $@ results/dotfiles.zip
	cd home && $(cosmos_zip_bin) -qr $(CURDIR)/$@ main.lua .args

home: results/bin/home

.PHONY: build test clean home
