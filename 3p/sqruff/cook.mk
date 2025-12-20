sqruff_dir := $(3p)/sqruff

sqruff_darwin_arm64_url := https://github.com/quarylabs/sqruff/releases/download/v0.21.2/sqruff-darwin-aarch64.tar.gz
sqruff_darwin_arm64_sha := cb969b42ebbca8229b4484ae2503530c4eef16e23829b340a0b270e1a007e6b6

sqruff_linux_arm64_url := https://github.com/quarylabs/sqruff/releases/download/v0.21.2/sqruff-linux-aarch64-musl.tar.gz
sqruff_linux_arm64_sha := 94ef0e55978a960f9cfc717bf5ed2127ae4462cc0a7915d7d38d843e3ca7ddfb

sqruff_linux_x86_64_url := https://github.com/quarylabs/sqruff/releases/download/v0.21.2/sqruff-linux-x86_64-musl.tar.gz
sqruff_linux_x86_64_sha := ae09dfcb0d275bf5317769d6eff8aa62c05942369f63ea5e747164a7db9225d9

$(sqruff_dir)/darwin-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(sqruff_dir)/darwin-arm64 \
	rw:/dev/null
$(sqruff_dir)/darwin-arm64/.extracted: | $(sqruff_dir)/darwin-arm64
	$(curl) -o $(sqruff_dir)/darwin-arm64/archive.tar.gz $(sqruff_darwin_arm64_url)
	cd $(sqruff_dir)/darwin-arm64 && echo "$(sqruff_darwin_arm64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(sqruff_dir)/darwin-arm64/archive.tar.gz -C $(sqruff_dir)/darwin-arm64
	rm $(sqruff_dir)/darwin-arm64/archive.tar.gz
	touch $@

$(sqruff_dir)/linux-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(sqruff_dir)/linux-arm64 \
	rw:/dev/null
$(sqruff_dir)/linux-arm64/.extracted: | $(sqruff_dir)/linux-arm64
	$(curl) -o $(sqruff_dir)/linux-arm64/archive.tar.gz $(sqruff_linux_arm64_url)
	cd $(sqruff_dir)/linux-arm64 && echo "$(sqruff_linux_arm64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(sqruff_dir)/linux-arm64/archive.tar.gz -C $(sqruff_dir)/linux-arm64
	rm $(sqruff_dir)/linux-arm64/archive.tar.gz
	touch $@

$(sqruff_dir)/linux-x86_64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(sqruff_dir)/linux-x86_64 \
	rw:/dev/null
$(sqruff_dir)/linux-x86_64/.extracted: | $(sqruff_dir)/linux-x86_64
	$(curl) -o $(sqruff_dir)/linux-x86_64/archive.tar.gz $(sqruff_linux_x86_64_url)
	cd $(sqruff_dir)/linux-x86_64 && echo "$(sqruff_linux_x86_64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(sqruff_dir)/linux-x86_64/archive.tar.gz -C $(sqruff_dir)/linux-x86_64
	rm $(sqruff_dir)/linux-x86_64/archive.tar.gz
	touch $@

$(sqruff_dir)/darwin-arm64:
	mkdir -p $@

$(sqruff_dir)/linux-arm64:
	mkdir -p $@

$(sqruff_dir)/linux-x86_64:
	mkdir -p $@

sqruff_binaries := \
	$(sqruff_dir)/darwin-arm64/.extracted \
	$(sqruff_dir)/linux-arm64/.extracted \
	$(sqruff_dir)/linux-x86_64/.extracted
