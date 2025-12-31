# cosmos binaries from whilp/cosmopolitan fork
cosmos_dir := $(o)/3p/cosmos
cosmos_version_file := lib/cosmos/version.lua

cosmos_lua_bin := $(cosmos_dir)/bin/lua
cosmos_zip_bin := $(cosmos_dir)/bin/zip
cosmos_unzip_bin := $(cosmos_dir)/bin/unzip
cosmos_make_bin := $(cosmos_dir)/bin/make
cosmos_bin := $(cosmos_make_bin)

$(cosmos_dir)/bin/%: lib/build/download-binary.lua $(cosmos_version_file) | $(lua_bin)
	$(lib_lua) lib/build/download-binary.lua $(cosmos_version_file) $* $@

cosmos: $(cosmos_lua_bin) $(cosmos_zip_bin) $(cosmos_unzip_bin) $(cosmos_make_bin)
.PHONY: cosmos
