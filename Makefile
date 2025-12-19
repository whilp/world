include 3p/cook.mk
include 3p/luaunit/cook.mk
include 3p/cosmopolitan/cook.mk
include 3p/lua/cook.mk
include 3p/shimlink/cook.mk

build: lua
test: lua
	$(lua_bin) 3p/lua/test.lua

clean:
	rm -rf o results

home_exclude_pattern = ^(3p/|o/|results/|Makefile|home/|\.git)

results/dotfiles.zip: | results
	git ls-files -z | grep -zZvE '$(home_exclude_pattern)' | \
		xargs -0 $(cosmos_zip_bin) -q $@

# Platform-specific binaries zips
results/binaries-darwin-arm64.zip: $(shimlink_binaries) | results
	cd $(shimlink_dir) && \
		find . -path '*/darwin-arm64/*' -type f ! -name '.extracted' | \
		$(cosmos_zip_bin) -q $(CURDIR)/$@ -@

results/binaries-linux-arm64.zip: $(shimlink_binaries) | results
	cd $(shimlink_dir) && \
		find . -path '*/linux-arm64/*' -type f ! -name '.extracted' | \
		$(cosmos_zip_bin) -q $(CURDIR)/$@ -@

results/binaries-linux-x86_64.zip: $(shimlink_binaries) | results
	cd $(shimlink_dir) && \
		find . -path '*/linux-x86_64/*' -type f ! -name '.extracted' | \
		$(cosmos_zip_bin) -q $(CURDIR)/$@ -@

# Platform-specific home binaries
results/bin/home-darwin-arm64: $(lua_bin) results/dotfiles.zip results/binaries-darwin-arm64.zip home/main.lua home/.args | results/bin
	cp $(lua_bin) $@
	$(cosmos_zip_bin) -qj $@ results/dotfiles.zip
	$(cosmos_zip_bin) -qj $@ results/binaries-darwin-arm64.zip
	cd home && $(cosmos_zip_bin) -qr $(CURDIR)/$@ main.lua .args

results/bin/home-linux-arm64: $(lua_bin) results/dotfiles.zip results/binaries-linux-arm64.zip home/main.lua home/.args | results/bin
	cp $(lua_bin) $@
	$(cosmos_zip_bin) -qj $@ results/dotfiles.zip
	$(cosmos_zip_bin) -qj $@ results/binaries-linux-arm64.zip
	cd home && $(cosmos_zip_bin) -qr $(CURDIR)/$@ main.lua .args

results/bin/home-linux-x86_64: $(lua_bin) results/dotfiles.zip results/binaries-linux-x86_64.zip home/main.lua home/.args | results/bin
	cp $(lua_bin) $@
	$(cosmos_zip_bin) -qj $@ results/dotfiles.zip
	$(cosmos_zip_bin) -qj $@ results/binaries-linux-x86_64.zip
	cd home && $(cosmos_zip_bin) -qr $(CURDIR)/$@ main.lua .args

results/bin:
	mkdir -p $@

results:
	mkdir -p $@

home: results/bin/home-darwin-arm64 results/bin/home-linux-arm64 results/bin/home-linux-x86_64

.PHONY: build test clean home
