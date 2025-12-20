rg_dir := $(3p)/rg

rg_darwin_arm64_url := https://github.com/BurntSushi/ripgrep/releases/download/14.1.1/ripgrep-14.1.1-aarch64-apple-darwin.tar.gz
rg_darwin_arm64_sha := 24ad76777745fbff131c8fbc466742b011f925bfa4fffa2ded6def23b5b937be

rg_linux_arm64_url := https://github.com/BurntSushi/ripgrep/releases/download/14.1.1/ripgrep-14.1.1-aarch64-unknown-linux-gnu.tar.gz
rg_linux_arm64_sha := c827481c4ff4ea10c9dc7a4022c8de5db34a5737cb74484d62eb94a95841ab2f

rg_linux_x86_64_url := https://github.com/BurntSushi/ripgrep/releases/download/14.1.1/ripgrep-14.1.1-x86_64-unknown-linux-musl.tar.gz
rg_linux_x86_64_sha := 4cf9f2741e6c465ffdb7c26f38056a59e2a2544b51f7cc128ef28337eeae4d8e

$(rg_dir)/darwin-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(rg_dir)/darwin-arm64 \
	rw:/dev/null
$(rg_dir)/darwin-arm64/.extracted: | $(rg_dir)/darwin-arm64
	$(curl) -o $(rg_dir)/darwin-arm64/archive.tar.gz $(rg_darwin_arm64_url)
	cd $(rg_dir)/darwin-arm64 && echo "$(rg_darwin_arm64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(rg_dir)/darwin-arm64/archive.tar.gz -C $(rg_dir)/darwin-arm64 --strip-components=1
	rm $(rg_dir)/darwin-arm64/archive.tar.gz
	touch $@

$(rg_dir)/linux-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(rg_dir)/linux-arm64 \
	rw:/dev/null
$(rg_dir)/linux-arm64/.extracted: | $(rg_dir)/linux-arm64
	$(curl) -o $(rg_dir)/linux-arm64/archive.tar.gz $(rg_linux_arm64_url)
	cd $(rg_dir)/linux-arm64 && echo "$(rg_linux_arm64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(rg_dir)/linux-arm64/archive.tar.gz -C $(rg_dir)/linux-arm64 --strip-components=1
	rm $(rg_dir)/linux-arm64/archive.tar.gz
	touch $@

$(rg_dir)/linux-x86_64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(rg_dir)/linux-x86_64 \
	rw:/dev/null
$(rg_dir)/linux-x86_64/.extracted: | $(rg_dir)/linux-x86_64
	$(curl) -o $(rg_dir)/linux-x86_64/archive.tar.gz $(rg_linux_x86_64_url)
	cd $(rg_dir)/linux-x86_64 && echo "$(rg_linux_x86_64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(rg_dir)/linux-x86_64/archive.tar.gz -C $(rg_dir)/linux-x86_64 --strip-components=1
	rm $(rg_dir)/linux-x86_64/archive.tar.gz
	touch $@

$(rg_dir)/darwin-arm64:
	mkdir -p $@

$(rg_dir)/linux-arm64:
	mkdir -p $@

$(rg_dir)/linux-x86_64:
	mkdir -p $@

rg_binaries := \
	$(rg_dir)/darwin-arm64/.extracted \
	$(rg_dir)/linux-arm64/.extracted \
	$(rg_dir)/linux-x86_64/.extracted
