ruff_dir := $(3p)/ruff

ruff_version := 0.8.4

ruff_darwin_arm64_url := https://github.com/astral-sh/ruff/releases/download/$(ruff_version)/ruff-aarch64-apple-darwin.tar.gz
ruff_darwin_arm64_sha := 8893f3ede33a73740f69b10ee9356e5cf2933c0afe146f00176be12ef91bf9d9

ruff_linux_arm64_url := https://github.com/astral-sh/ruff/releases/download/$(ruff_version)/ruff-aarch64-unknown-linux-gnu.tar.gz
ruff_linux_arm64_sha := 0dfe36fabb817638863375e0140ce03bf26ccc9a7fd9d2c8e8337b1a21697ed4

ruff_linux_x86_64_url := https://github.com/astral-sh/ruff/releases/download/$(ruff_version)/ruff-x86_64-unknown-linux-gnu.tar.gz
ruff_linux_x86_64_sha := c4e6591ae1bb4f15c09c9022b7bfc57e1c3a567acdc9cd76021cd1304b5868c3

$(ruff_dir)/darwin-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(ruff_dir)/darwin-arm64 \
	rw:/dev/null
$(ruff_dir)/darwin-arm64/.extracted: | $(ruff_dir)/darwin-arm64
	$(curl) -o $(ruff_dir)/darwin-arm64/archive.tar.gz $(ruff_darwin_arm64_url)
	cd $(ruff_dir)/darwin-arm64 && echo "$(ruff_darwin_arm64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(ruff_dir)/darwin-arm64/archive.tar.gz -C $(ruff_dir)/darwin-arm64 --strip-components=1
	rm $(ruff_dir)/darwin-arm64/archive.tar.gz
	touch $@

$(ruff_dir)/darwin-arm64/VERSION: $(ruff_dir)/darwin-arm64/.extracted
	echo "$(ruff_version)" > $@

$(ruff_dir)/darwin-arm64/SHA: $(ruff_dir)/darwin-arm64/.extracted
	echo "$(ruff_darwin_arm64_sha)" | head -c 8 > $@

$(ruff_dir)/linux-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(ruff_dir)/linux-arm64 \
	rw:/dev/null
$(ruff_dir)/linux-arm64/.extracted: | $(ruff_dir)/linux-arm64
	$(curl) -o $(ruff_dir)/linux-arm64/archive.tar.gz $(ruff_linux_arm64_url)
	cd $(ruff_dir)/linux-arm64 && echo "$(ruff_linux_arm64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(ruff_dir)/linux-arm64/archive.tar.gz -C $(ruff_dir)/linux-arm64 --strip-components=1
	rm $(ruff_dir)/linux-arm64/archive.tar.gz
	touch $@

$(ruff_dir)/linux-arm64/VERSION: $(ruff_dir)/linux-arm64/.extracted
	echo "$(ruff_version)" > $@

$(ruff_dir)/linux-arm64/SHA: $(ruff_dir)/linux-arm64/.extracted
	echo "$(ruff_linux_arm64_sha)" | head -c 8 > $@

$(ruff_dir)/linux-x86_64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(ruff_dir)/linux-x86_64 \
	rw:/dev/null
$(ruff_dir)/linux-x86_64/.extracted: | $(ruff_dir)/linux-x86_64
	$(curl) -o $(ruff_dir)/linux-x86_64/archive.tar.gz $(ruff_linux_x86_64_url)
	cd $(ruff_dir)/linux-x86_64 && echo "$(ruff_linux_x86_64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(ruff_dir)/linux-x86_64/archive.tar.gz -C $(ruff_dir)/linux-x86_64 --strip-components=1
	rm $(ruff_dir)/linux-x86_64/archive.tar.gz
	touch $@

$(ruff_dir)/linux-x86_64/VERSION: $(ruff_dir)/linux-x86_64/.extracted
	echo "$(ruff_version)" > $@

$(ruff_dir)/linux-x86_64/SHA: $(ruff_dir)/linux-x86_64/.extracted
	echo "$(ruff_linux_x86_64_sha)" | head -c 8 > $@

$(ruff_dir)/darwin-arm64:
	mkdir -p $@

$(ruff_dir)/linux-arm64:
	mkdir -p $@

$(ruff_dir)/linux-x86_64:
	mkdir -p $@

ruff_binaries := \
	$(ruff_dir)/darwin-arm64/.extracted \
	$(ruff_dir)/darwin-arm64/VERSION \
	$(ruff_dir)/darwin-arm64/SHA \
	$(ruff_dir)/linux-arm64/.extracted \
	$(ruff_dir)/linux-arm64/VERSION \
	$(ruff_dir)/linux-arm64/SHA \
	$(ruff_dir)/linux-x86_64/.extracted \
	$(ruff_dir)/linux-x86_64/VERSION \
	$(ruff_dir)/linux-x86_64/SHA
