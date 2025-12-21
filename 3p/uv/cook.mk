uv_dir := $(3p)/uv

uv_version := 0.5.7

uv_darwin_arm64_url := https://github.com/astral-sh/uv/releases/download/$(uv_version)/uv-aarch64-apple-darwin.tar.gz
uv_darwin_arm64_sha := b8cab25ab2ec0714dbb34179f948c27aa4ab307be54e0628e9e1eef1d2264f9f

uv_linux_arm64_url := https://github.com/astral-sh/uv/releases/download/$(uv_version)/uv-aarch64-unknown-linux-gnu.tar.gz
uv_linux_arm64_sha := d4dd7a72689888c92b5191902fd4ec9d25b7eeba07be41ba4a8f89acbb403e2d

uv_linux_x86_64_url := https://github.com/astral-sh/uv/releases/download/$(uv_version)/uv-x86_64-unknown-linux-gnu.tar.gz
uv_linux_x86_64_sha := 8a0a3e823684dec6e49ae17f31bf6483c778fd579671992d9156875210e5161e

$(uv_dir)/darwin-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(uv_dir)/darwin-arm64 \
	rw:/dev/null
$(uv_dir)/darwin-arm64/.extracted: | $(uv_dir)/darwin-arm64
	$(curl) -o $(uv_dir)/darwin-arm64/archive.tar.gz $(uv_darwin_arm64_url)
	cd $(uv_dir)/darwin-arm64 && echo "$(uv_darwin_arm64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(uv_dir)/darwin-arm64/archive.tar.gz -C $(uv_dir)/darwin-arm64 --strip-components=1
	rm $(uv_dir)/darwin-arm64/archive.tar.gz
	echo "$(uv_version)" > $(uv_dir)/darwin-arm64/VERSION
	echo "$(uv_darwin_arm64_sha)" | head -c 8 > $(uv_dir)/darwin-arm64/SHA
	touch $@

$(uv_dir)/linux-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(uv_dir)/linux-arm64 \
	rw:/dev/null
$(uv_dir)/linux-arm64/.extracted: | $(uv_dir)/linux-arm64
	$(curl) -o $(uv_dir)/linux-arm64/archive.tar.gz $(uv_linux_arm64_url)
	cd $(uv_dir)/linux-arm64 && echo "$(uv_linux_arm64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(uv_dir)/linux-arm64/archive.tar.gz -C $(uv_dir)/linux-arm64 --strip-components=1
	rm $(uv_dir)/linux-arm64/archive.tar.gz
	echo "$(uv_version)" > $(uv_dir)/linux-arm64/VERSION
	echo "$(uv_linux_arm64_sha)" | head -c 8 > $(uv_dir)/linux-arm64/SHA
	touch $@

$(uv_dir)/linux-x86_64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(uv_dir)/linux-x86_64 \
	rw:/dev/null
$(uv_dir)/linux-x86_64/.extracted: | $(uv_dir)/linux-x86_64
	$(curl) -o $(uv_dir)/linux-x86_64/archive.tar.gz $(uv_linux_x86_64_url)
	cd $(uv_dir)/linux-x86_64 && echo "$(uv_linux_x86_64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(uv_dir)/linux-x86_64/archive.tar.gz -C $(uv_dir)/linux-x86_64 --strip-components=1
	rm $(uv_dir)/linux-x86_64/archive.tar.gz
	echo "$(uv_version)" > $(uv_dir)/linux-x86_64/VERSION
	echo "$(uv_linux_x86_64_sha)" | head -c 8 > $(uv_dir)/linux-x86_64/SHA
	touch $@

$(uv_dir)/darwin-arm64:
	mkdir -p $@

$(uv_dir)/linux-arm64:
	mkdir -p $@

$(uv_dir)/linux-x86_64:
	mkdir -p $@

uv_binaries := \
	$(uv_dir)/darwin-arm64/.extracted \
	$(uv_dir)/linux-arm64/.extracted \
	$(uv_dir)/linux-x86_64/.extracted
