gh_dir := $(3p)/gh

gh_version := 2.79.0

gh_darwin_arm64_url := https://github.com/cli/cli/releases/download/v$(gh_version)/gh_$(gh_version)_macOS_arm64.zip
gh_darwin_arm64_sha := 5454f9509e3dbb8f321310e9e344877d9a01ebb8f8703886b1afb0936d60ffaa

gh_linux_arm64_url := https://github.com/cli/cli/releases/download/v$(gh_version)/gh_$(gh_version)_linux_arm64.tar.gz
gh_linux_arm64_sha := 1b91e546b30181a8ee6d8c72bbf59eaadbb0600bab014dfbcc199676c83ea102

gh_linux_x86_64_url := https://github.com/cli/cli/releases/download/v$(gh_version)/gh_$(gh_version)_linux_amd64.tar.gz
gh_linux_x86_64_sha := e7af0c72a607c0528fda1989f7c8e3be85e67d321889002af0e2938ad9c8fb68

$(gh_dir)/darwin-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(gh_dir)/darwin-arm64 \
	rw:/dev/null
$(gh_dir)/darwin-arm64/.extracted: | $(gh_dir)/darwin-arm64
	$(curl) -o $(gh_dir)/darwin-arm64/archive.zip $(gh_darwin_arm64_url)
	cd $(gh_dir)/darwin-arm64 && echo "$(gh_darwin_arm64_sha)  archive.zip" | $(sha256sum) -c
	$(unzip) -o $(gh_dir)/darwin-arm64/archive.zip -d $(gh_dir)/darwin-arm64
	find $(gh_dir)/darwin-arm64 -mindepth 2 -maxdepth 2 -exec mv {} $(gh_dir)/darwin-arm64/ \;
	find $(gh_dir)/darwin-arm64 -mindepth 1 -maxdepth 1 -type d -empty -delete
	rm $(gh_dir)/darwin-arm64/archive.zip
	touch $@

$(gh_dir)/darwin-arm64/VERSION: $(gh_dir)/darwin-arm64/.extracted
	echo "$(gh_version)" > $@

$(gh_dir)/darwin-arm64/SHA: $(gh_dir)/darwin-arm64/.extracted
	echo "$(gh_darwin_arm64_sha)" | head -c 8 > $@

$(gh_dir)/linux-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(gh_dir)/linux-arm64 \
	rw:/dev/null
$(gh_dir)/linux-arm64/.extracted: | $(gh_dir)/linux-arm64
	$(curl) -o $(gh_dir)/linux-arm64/archive.tar.gz $(gh_linux_arm64_url)
	cd $(gh_dir)/linux-arm64 && echo "$(gh_linux_arm64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(gh_dir)/linux-arm64/archive.tar.gz -C $(gh_dir)/linux-arm64 --strip-components=1
	rm $(gh_dir)/linux-arm64/archive.tar.gz
	touch $@

$(gh_dir)/linux-arm64/VERSION: $(gh_dir)/linux-arm64/.extracted
	echo "$(gh_version)" > $@

$(gh_dir)/linux-arm64/SHA: $(gh_dir)/linux-arm64/.extracted
	echo "$(gh_linux_arm64_sha)" | head -c 8 > $@

$(gh_dir)/linux-x86_64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(gh_dir)/linux-x86_64 \
	rw:/dev/null
$(gh_dir)/linux-x86_64/.extracted: | $(gh_dir)/linux-x86_64
	$(curl) -o $(gh_dir)/linux-x86_64/archive.tar.gz $(gh_linux_x86_64_url)
	cd $(gh_dir)/linux-x86_64 && echo "$(gh_linux_x86_64_sha)  archive.tar.gz" | $(sha256sum) -c
	$(tar) -xzf $(gh_dir)/linux-x86_64/archive.tar.gz -C $(gh_dir)/linux-x86_64 --strip-components=1
	rm $(gh_dir)/linux-x86_64/archive.tar.gz
	touch $@

$(gh_dir)/linux-x86_64/VERSION: $(gh_dir)/linux-x86_64/.extracted
	echo "$(gh_version)" > $@

$(gh_dir)/linux-x86_64/SHA: $(gh_dir)/linux-x86_64/.extracted
	echo "$(gh_linux_x86_64_sha)" | head -c 8 > $@

$(gh_dir)/darwin-arm64:
	mkdir -p $@

$(gh_dir)/linux-arm64:
	mkdir -p $@

$(gh_dir)/linux-x86_64:
	mkdir -p $@

gh_binaries := \
	$(gh_dir)/darwin-arm64/.extracted \
	$(gh_dir)/darwin-arm64/VERSION \
	$(gh_dir)/darwin-arm64/SHA \
	$(gh_dir)/linux-arm64/.extracted \
	$(gh_dir)/linux-arm64/VERSION \
	$(gh_dir)/linux-arm64/SHA \
	$(gh_dir)/linux-x86_64/.extracted \
	$(gh_dir)/linux-x86_64/VERSION \
	$(gh_dir)/linux-x86_64/SHA
