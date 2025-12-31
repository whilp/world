# cosmos binaries from whilp/cosmopolitan fork
cosmos_dir := $(o)/any/3p/cosmos
cosmos_version := lib/cosmos/version.lua
cosmos_fetch := lib/build/fetch.lua

cosmos_lua_bin := $(cosmos_dir)/bin/lua
cosmos_zip_bin := $(cosmos_dir)/bin/zip
cosmos_unzip_bin := $(cosmos_dir)/bin/unzip
cosmos_make_bin := $(cosmos_dir)/bin/make

cosmos_bin := $(cosmos_make_bin)

$(cosmos_lua_bin): private .UNVEIL = r:/etc/resolv.conf r:/etc/ssl r:lib rwc:$(cosmos_dir) rw:/dev/null
$(cosmos_lua_bin): private .PLEDGE = stdio rpath wpath cpath inet dns
$(cosmos_lua_bin): private .INTERNET = 1
$(cosmos_lua_bin): $(cosmos_fetch) $(cosmos_version)
	$(lua_bin) $(cosmos_fetch) $(cosmos_version) lua $@

$(cosmos_zip_bin): private .UNVEIL = r:/etc/resolv.conf r:/etc/ssl r:lib rwc:$(cosmos_dir) rw:/dev/null
$(cosmos_zip_bin): private .PLEDGE = stdio rpath wpath cpath inet dns
$(cosmos_zip_bin): private .INTERNET = 1
$(cosmos_zip_bin): $(cosmos_fetch) $(cosmos_version)
	$(lua_bin) $(cosmos_fetch) $(cosmos_version) zip $@

$(cosmos_unzip_bin): private .UNVEIL = r:/etc/resolv.conf r:/etc/ssl r:lib rwc:$(cosmos_dir) rw:/dev/null
$(cosmos_unzip_bin): private .PLEDGE = stdio rpath wpath cpath inet dns
$(cosmos_unzip_bin): private .INTERNET = 1
$(cosmos_unzip_bin): $(cosmos_fetch) $(cosmos_version)
	$(lua_bin) $(cosmos_fetch) $(cosmos_version) unzip $@

$(cosmos_make_bin): private .UNVEIL = r:/etc/resolv.conf r:/etc/ssl r:lib rwc:$(cosmos_dir) rw:/dev/null
$(cosmos_make_bin): private .PLEDGE = stdio rpath wpath cpath inet dns
$(cosmos_make_bin): private .INTERNET = 1
$(cosmos_make_bin): $(cosmos_fetch) $(cosmos_version)
	$(lua_bin) $(cosmos_fetch) $(cosmos_version) make $@

cosmos: $(cosmos_lua_bin) $(cosmos_zip_bin) $(cosmos_unzip_bin) $(cosmos_make_bin)
.PHONY: cosmos
