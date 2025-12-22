delta_dir := $(3p)/delta

delta_version := 0.18.2

delta_darwin_arm64_url := https://github.com/dandavison/delta/releases/download/$(delta_version)/delta-$(delta_version)-aarch64-apple-darwin.tar.gz
delta_darwin_arm64_sha := 6ba38dce9f91ee1b9a24aa4aede1db7195258fe176c3f8276ae2d4457d8170a0

delta_linux_arm64_url := https://github.com/dandavison/delta/releases/download/$(delta_version)/delta-$(delta_version)-aarch64-unknown-linux-gnu.tar.gz
delta_linux_arm64_sha := adf7674086daa4582f598f74ce9caa6b70c1ba8f4a57d2911499b37826b014f9

delta_linux_x86_64_url := https://github.com/dandavison/delta/releases/download/$(delta_version)/delta-$(delta_version)-x86_64-unknown-linux-musl.tar.gz
delta_linux_x86_64_sha := b7ea845004762358a00ef9127dd9fd723e333c7e4b9cb1da220c3909372310ee

$(delta_dir)/darwin-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(delta_dir)/darwin-arm64 \
	rw:/dev/null
$(delta_dir)/darwin-arm64/.extracted: | $(delta_dir)/darwin-arm64
	$(curl) -o $(delta_dir)/darwin-arm64/archive.tar.gz $(delta_darwin_arm64_url)
	cd $(delta_dir)/darwin-arm64 && echo "$(delta_darwin_arm64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(delta_dir)/darwin-arm64/archive.tar.gz -C $(delta_dir)/darwin-arm64 --strip-components=1
	rm $(delta_dir)/darwin-arm64/archive.tar.gz
	touch $@

$(delta_dir)/darwin-arm64/VERSION: $(delta_dir)/darwin-arm64/.extracted
	echo "$(delta_version)" > $@

$(delta_dir)/darwin-arm64/SHA: $(delta_dir)/darwin-arm64/.extracted
	echo "$(delta_darwin_arm64_sha)" | head -c 8 > $@

$(delta_dir)/linux-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(delta_dir)/linux-arm64 \
	rw:/dev/null
$(delta_dir)/linux-arm64/.extracted: | $(delta_dir)/linux-arm64
	$(curl) -o $(delta_dir)/linux-arm64/archive.tar.gz $(delta_linux_arm64_url)
	cd $(delta_dir)/linux-arm64 && echo "$(delta_linux_arm64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(delta_dir)/linux-arm64/archive.tar.gz -C $(delta_dir)/linux-arm64 --strip-components=1
	rm $(delta_dir)/linux-arm64/archive.tar.gz
	touch $@

$(delta_dir)/linux-arm64/VERSION: $(delta_dir)/linux-arm64/.extracted
	echo "$(delta_version)" > $@

$(delta_dir)/linux-arm64/SHA: $(delta_dir)/linux-arm64/.extracted
	echo "$(delta_linux_arm64_sha)" | head -c 8 > $@

$(delta_dir)/linux-x86_64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(delta_dir)/linux-x86_64 \
	rw:/dev/null
$(delta_dir)/linux-x86_64/.extracted: | $(delta_dir)/linux-x86_64
	$(curl) -o $(delta_dir)/linux-x86_64/archive.tar.gz $(delta_linux_x86_64_url)
	cd $(delta_dir)/linux-x86_64 && echo "$(delta_linux_x86_64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(delta_dir)/linux-x86_64/archive.tar.gz -C $(delta_dir)/linux-x86_64 --strip-components=1
	rm $(delta_dir)/linux-x86_64/archive.tar.gz
	touch $@

$(delta_dir)/linux-x86_64/VERSION: $(delta_dir)/linux-x86_64/.extracted
	echo "$(delta_version)" > $@

$(delta_dir)/linux-x86_64/SHA: $(delta_dir)/linux-x86_64/.extracted
	echo "$(delta_linux_x86_64_sha)" | head -c 8 > $@

$(delta_dir)/darwin-arm64:
	mkdir -p $@

$(delta_dir)/linux-arm64:
	mkdir -p $@

$(delta_dir)/linux-x86_64:
	mkdir -p $@

delta_binaries := \
	$(delta_dir)/darwin-arm64/.extracted \
	$(delta_dir)/darwin-arm64/VERSION \
	$(delta_dir)/darwin-arm64/SHA \
	$(delta_dir)/linux-arm64/.extracted \
	$(delta_dir)/linux-arm64/VERSION \
	$(delta_dir)/linux-arm64/SHA \
	$(delta_dir)/linux-x86_64/.extracted \
	$(delta_dir)/linux-x86_64/VERSION \
	$(delta_dir)/linux-x86_64/SHA
