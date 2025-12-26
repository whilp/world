# pre-built lua from dotfiles releases
# to update: set version, then run:
#   curl -fsSL https://github.com/whilp/dotfiles/releases/download/<version>/SHA256SUMS | grep lua
lua_release_dir := $(3p)/lua-release
lua_release_bin := $(lua_release_dir)/lua
lua_release_version := home-2025-12-25-c11d85b
lua_release_url := https://github.com/whilp/dotfiles/releases/download/$(lua_release_version)/lua
lua_release_sha := b1b144c389f538fabdbd7f47ca771cacada794fca179ed89969253abef60c286

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
