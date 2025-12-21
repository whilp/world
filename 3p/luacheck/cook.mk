luacheck_url := https://github.com/lunarmodules/luacheck/archive/refs/tags/v1.2.0.tar.gz
luacheck_sha256 := 8efe62a7da4fdb32c0c22ec1f7c9306cbc397d7d40493c29988221a059636e25
luacheck_dir := $(3p)/luacheck
luacheck_archive := $(luacheck_dir)/luacheck-1.2.0.tar.gz
luacheck_src := $(luacheck_dir)/luacheck-1.2.0
luacheck_lua_dir := $(luacheck_dir)/.lua

argparse_url := https://github.com/mpeterv/argparse/archive/refs/tags/0.6.0.tar.gz
argparse_sha256 := 0eddda29d591536bc7310b99ce7acc3e5e00557f18d6e63ab10d56683e8952f1
argparse_archive := $(luacheck_dir)/argparse-0.6.0.tar.gz
argparse_src := $(luacheck_dir)/argparse-0.6.0

$(luacheck_archive): | $(luacheck_dir)
	$(curl) -L -o $@ $(luacheck_url)
	cd $(dir $@) && echo "$(luacheck_sha256)  $(notdir $@)" | $(sha256sum) -c

$(luacheck_src): $(luacheck_archive)
	cd $(luacheck_dir) && tar -xzf $(notdir $(luacheck_archive))

$(argparse_archive): | $(luacheck_dir)
	$(curl) -L -o $@ $(argparse_url)
	cd $(dir $@) && echo "$(argparse_sha256)  $(notdir $@)" | $(sha256sum) -c

$(argparse_src): $(argparse_archive)
	cd $(luacheck_dir) && tar -xzf $(notdir $(argparse_archive))

$(luacheck_lua_dir)/bin/luacheck: $(luacheck_src) $(argparse_src)
	mkdir -p $(luacheck_lua_dir)/bin
	cp -r $(luacheck_src)/src/luacheck $(luacheck_lua_dir)/
	cp $(argparse_src)/src/argparse.lua $(luacheck_lua_dir)/
	cp $(luacheck_src)/bin/luacheck.lua $(luacheck_lua_dir)/bin/luacheck
	cp 3p/lua/lfs_stub.lua $(luacheck_lua_dir)/lfs.lua

$(luacheck_dir):
	mkdir -p $@

luacheck_libs := $(luacheck_lua_dir)
