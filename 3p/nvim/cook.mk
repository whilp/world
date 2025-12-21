nvim_dir := $(3p)/nvim

# Pinned to neovim nightly from 2025-12-07
# Upstream commit: c016a4c
nvim_upstream_sha := c016a4c

nvim_darwin_arm64_url := https://github.com/whilp/dotfiles/releases/download/2025.12.07-c016a4c/nvim-2025.12.07-darwin-arm64.tar.gz
nvim_darwin_arm64_sha := 143513b8f91dd29a510beef8c1202a9c623f5ced2f0f379df894d1d3e4b37039

nvim_linux_arm64_url := https://github.com/whilp/dotfiles/releases/download/2025.12.07-c016a4c/nvim-2025.12.07-linux-arm64.tar.gz
nvim_linux_arm64_sha := 4a1101efbf237749c0727c356bc3dcf78be6fdbae27d63fc9a5d147b0808a821

nvim_linux_x86_64_url := https://github.com/whilp/dotfiles/releases/download/2025.12.07-c016a4c/nvim-2025.12.07-linux-x64.tar.gz
nvim_linux_x86_64_sha := 92b09500a845d5c5dd35473b28486c188a836ccc4fa3ab7fe54d2ce0777b4e0d

$(nvim_dir)/darwin-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(nvim_dir)/darwin-arm64 \
	rw:/dev/null
$(nvim_dir)/darwin-arm64/.extracted: | $(nvim_dir)/darwin-arm64
	$(curl) -o $(nvim_dir)/darwin-arm64/archive.tar.gz $(nvim_darwin_arm64_url)
	cd $(nvim_dir)/darwin-arm64 && echo "$(nvim_darwin_arm64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(nvim_dir)/darwin-arm64/archive.tar.gz -C $(nvim_dir)/darwin-arm64 --strip-components=1
	rm $(nvim_dir)/darwin-arm64/archive.tar.gz
	touch $@

$(nvim_dir)/linux-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(nvim_dir)/linux-arm64 \
	rw:/dev/null
$(nvim_dir)/linux-arm64/.extracted: | $(nvim_dir)/linux-arm64
	$(curl) -o $(nvim_dir)/linux-arm64/archive.tar.gz $(nvim_linux_arm64_url)
	cd $(nvim_dir)/linux-arm64 && echo "$(nvim_linux_arm64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(nvim_dir)/linux-arm64/archive.tar.gz -C $(nvim_dir)/linux-arm64 --strip-components=1
	rm $(nvim_dir)/linux-arm64/archive.tar.gz
	touch $@

$(nvim_dir)/linux-x86_64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(nvim_dir)/linux-x86_64 \
	rw:/dev/null
$(nvim_dir)/linux-x86_64/.extracted: | $(nvim_dir)/linux-x86_64
	$(curl) -o $(nvim_dir)/linux-x86_64/archive.tar.gz $(nvim_linux_x86_64_url)
	cd $(nvim_dir)/linux-x86_64 && echo "$(nvim_linux_x86_64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(nvim_dir)/linux-x86_64/archive.tar.gz -C $(nvim_dir)/linux-x86_64 --strip-components=1
	rm $(nvim_dir)/linux-x86_64/archive.tar.gz
	touch $@

$(nvim_dir)/darwin-arm64:
	mkdir -p $@

$(nvim_dir)/linux-arm64:
	mkdir -p $@

$(nvim_dir)/linux-x86_64:
	mkdir -p $@

nvim_binaries := \
	$(nvim_dir)/darwin-arm64/.extracted \
	$(nvim_dir)/linux-arm64/.extracted \
	$(nvim_dir)/linux-x86_64/.extracted

# Plugin bundling via nvim headless with vim.pack.add
nvim_pack_lock := .config/nvim/nvim-pack-lock.json
nvim_bundle_script := 3p/nvim/bundle-plugins.lua

$(nvim_dir)/%/.plugins_bundled: $(nvim_dir)/%/.extracted $(nvim_pack_lock) $(nvim_bundle_script)
	@echo "bundling plugins for platform $*"
	XDG_DATA_HOME=$(nvim_dir)/$*/share \
	NVIM_PACK_LOCK=$(nvim_pack_lock) \
	$(nvim_dir)/$*/bin/nvim --headless \
	  -c "luafile $(nvim_bundle_script)" \
	  -c "qa"
	@touch $@

# Phony target for workflow to call
PLATFORM ?= linux-x86_64
.PHONY: nvim-bundle-plugins
nvim-bundle-plugins: $(nvim_dir)/$(PLATFORM)/.plugins_bundled
