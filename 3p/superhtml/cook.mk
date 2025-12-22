superhtml_dir := $(3p)/superhtml

superhtml_version := 0.5.3

superhtml_darwin_arm64_url := https://github.com/kristoff-it/superhtml/releases/download/v$(superhtml_version)/aarch64-macos.tar.gz
superhtml_darwin_arm64_sha := b8b2327f666ff316422061284e107add5c413ebdfdb91774c0c3702a66e65ec9

superhtml_linux_arm64_url := https://github.com/kristoff-it/superhtml/releases/download/v$(superhtml_version)/aarch64-linux.tar.gz
superhtml_linux_arm64_sha := 54cd2414de6664b85166a0a2e7c208ca3dbcc935274f4a55309cc9dcfa8e605b

superhtml_linux_x86_64_url := https://github.com/kristoff-it/superhtml/releases/download/v$(superhtml_version)/x86_64-linux-musl.tar.gz
superhtml_linux_x86_64_sha := c9fabbbd57851e38a67e6c1eb7942e8bc6189925bfcf437f1e5286932c76d60a

$(superhtml_dir)/darwin-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(superhtml_dir)/darwin-arm64 \
	rw:/dev/null
$(superhtml_dir)/darwin-arm64/.extracted: | $(superhtml_dir)/darwin-arm64
	$(curl) -o $(superhtml_dir)/darwin-arm64/archive.tar.gz $(superhtml_darwin_arm64_url)
	cd $(superhtml_dir)/darwin-arm64 && echo "$(superhtml_darwin_arm64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(superhtml_dir)/darwin-arm64/archive.tar.gz -C $(superhtml_dir)/darwin-arm64 --strip-components=1
	rm $(superhtml_dir)/darwin-arm64/archive.tar.gz
	touch $@

$(superhtml_dir)/darwin-arm64/VERSION: $(superhtml_dir)/darwin-arm64/.extracted
	echo "$(superhtml_version)" > $@

$(superhtml_dir)/darwin-arm64/SHA: $(superhtml_dir)/darwin-arm64/.extracted
	echo "$(superhtml_darwin_arm64_sha)" | head -c 8 > $@

$(superhtml_dir)/linux-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(superhtml_dir)/linux-arm64 \
	rw:/dev/null
$(superhtml_dir)/linux-arm64/.extracted: | $(superhtml_dir)/linux-arm64
	$(curl) -o $(superhtml_dir)/linux-arm64/archive.tar.gz $(superhtml_linux_arm64_url)
	cd $(superhtml_dir)/linux-arm64 && echo "$(superhtml_linux_arm64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(superhtml_dir)/linux-arm64/archive.tar.gz -C $(superhtml_dir)/linux-arm64 --strip-components=1
	rm $(superhtml_dir)/linux-arm64/archive.tar.gz
	touch $@

$(superhtml_dir)/linux-arm64/VERSION: $(superhtml_dir)/linux-arm64/.extracted
	echo "$(superhtml_version)" > $@

$(superhtml_dir)/linux-arm64/SHA: $(superhtml_dir)/linux-arm64/.extracted
	echo "$(superhtml_linux_arm64_sha)" | head -c 8 > $@

$(superhtml_dir)/linux-x86_64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(superhtml_dir)/linux-x86_64 \
	rw:/dev/null
$(superhtml_dir)/linux-x86_64/.extracted: | $(superhtml_dir)/linux-x86_64
	$(curl) -o $(superhtml_dir)/linux-x86_64/archive.tar.gz $(superhtml_linux_x86_64_url)
	cd $(superhtml_dir)/linux-x86_64 && echo "$(superhtml_linux_x86_64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(superhtml_dir)/linux-x86_64/archive.tar.gz -C $(superhtml_dir)/linux-x86_64 --strip-components=1
	rm $(superhtml_dir)/linux-x86_64/archive.tar.gz
	touch $@

$(superhtml_dir)/linux-x86_64/VERSION: $(superhtml_dir)/linux-x86_64/.extracted
	echo "$(superhtml_version)" > $@

$(superhtml_dir)/linux-x86_64/SHA: $(superhtml_dir)/linux-x86_64/.extracted
	echo "$(superhtml_linux_x86_64_sha)" | head -c 8 > $@

$(superhtml_dir)/darwin-arm64:
	mkdir -p $@

$(superhtml_dir)/linux-arm64:
	mkdir -p $@

$(superhtml_dir)/linux-x86_64:
	mkdir -p $@

superhtml_binaries := \
	$(superhtml_dir)/darwin-arm64/.extracted \
	$(superhtml_dir)/darwin-arm64/VERSION \
	$(superhtml_dir)/darwin-arm64/SHA \
	$(superhtml_dir)/linux-arm64/.extracted \
	$(superhtml_dir)/linux-arm64/VERSION \
	$(superhtml_dir)/linux-arm64/SHA \
	$(superhtml_dir)/linux-x86_64/.extracted \
	$(superhtml_dir)/linux-x86_64/VERSION \
	$(superhtml_dir)/linux-x86_64/SHA
