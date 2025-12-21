nvim_dir := $(3p)/nvim

nvim_version := 2025.12.07
nvim_release_sha := c016a4c

nvim_darwin_arm64_url := https://github.com/whilp/dotfiles/releases/download/$(nvim_version)-$(nvim_release_sha)/nvim-$(nvim_version)-darwin-arm64.tar.gz
nvim_darwin_arm64_sha := 143513b8f91dd29a510beef8c1202a9c623f5ced2f0f379df894d1d3e4b37039

nvim_linux_arm64_url := https://github.com/whilp/dotfiles/releases/download/$(nvim_version)-$(nvim_release_sha)/nvim-$(nvim_version)-linux-arm64.tar.gz
nvim_linux_arm64_sha := 4a1101efbf237749c0727c356bc3dcf78be6fdbae27d63fc9a5d147b0808a821

nvim_linux_x86_64_url := https://github.com/whilp/dotfiles/releases/download/$(nvim_version)-$(nvim_release_sha)/nvim-$(nvim_version)-linux-x64.tar.gz
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
	echo "$(nvim_version)" > $(nvim_dir)/darwin-arm64/VERSION
	echo "$(nvim_release_sha)" > $(nvim_dir)/darwin-arm64/SHA
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
	echo "$(nvim_version)" > $(nvim_dir)/linux-arm64/VERSION
	echo "$(nvim_release_sha)" > $(nvim_dir)/linux-arm64/SHA
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
	echo "$(nvim_version)" > $(nvim_dir)/linux-x86_64/VERSION
	echo "$(nvim_release_sha)" > $(nvim_dir)/linux-x86_64/SHA
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
