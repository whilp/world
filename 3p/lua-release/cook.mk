# pre-built lua from dotfiles releases
# to update: set version, then run:
#   curl -fsSL https://github.com/whilp/dotfiles/releases/download/<version>/SHA256SUMS | grep lua
lua_release_dir := $(3p)/lua-release
lua_release_bin := $(lua_release_dir)/lua
lua_release_version := home-2025-12-25-c11d85b
lua_release_url := https://github.com/whilp/dotfiles/releases/download/$(lua_release_version)/lua
lua_release_sha := e2f6ff7a1c7f85e87b6014d60ac17eeda9b94f0be7b31acad0acd8ac1ea4bdb4

$(lua_release_bin): private .UNVEIL = r:/etc/resolv.conf r:/etc/ssl rwc:$(lua_release_dir) rw:/dev/null
$(lua_release_bin): private .PLEDGE = stdio rpath wpath cpath inet dns
$(lua_release_bin): private .INTERNET = 1
$(lua_release_bin): | $(lua_release_dir)
	$(curl) -o $@ $(lua_release_url)
	cd $(dir $@) && echo "$(lua_release_sha)  $(notdir $@)" | $(sha256sum) -c
	chmod +x $@

$(lua_release_dir):
	mkdir -p $@

lua-release: $(lua_release_bin)

clean-lua-release:
	rm -rf $(lua_release_dir)

.PHONY: lua-release clean-lua-release
