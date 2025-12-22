ast_grep_dir := $(3p)/ast-grep

ast_grep_version := 0.40.3

ast_grep_darwin_arm64_url := https://github.com/ast-grep/ast-grep/releases/download/$(ast_grep_version)/app-aarch64-apple-darwin.zip
ast_grep_darwin_arm64_sha := 4fda598391d0ad819e23de1355a3c1e16fe5aa4056ae90410321260cd1ba6f8b

ast_grep_linux_arm64_url := https://github.com/ast-grep/ast-grep/releases/download/$(ast_grep_version)/app-aarch64-unknown-linux-gnu.zip
ast_grep_linux_arm64_sha := dd409e779752cd68f1afe9437c9f195245290d26d5293aa052c6c759dcfbddd1

ast_grep_linux_x86_64_url := https://github.com/ast-grep/ast-grep/releases/download/$(ast_grep_version)/app-x86_64-unknown-linux-gnu.zip
ast_grep_linux_x86_64_sha := 253c94dc566652662cb1efdad86a08689578a3dcfbd7d7c03e4c8a73de79ba5b

$(ast_grep_dir)/darwin-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(ast_grep_dir)/darwin-arm64 \
	rw:/dev/null
$(ast_grep_dir)/darwin-arm64/.extracted: | $(ast_grep_dir)/darwin-arm64
	$(curl) -o $(ast_grep_dir)/darwin-arm64/archive.zip $(ast_grep_darwin_arm64_url)
	cd $(ast_grep_dir)/darwin-arm64 && echo "$(ast_grep_darwin_arm64_sha)  archive.zip" | $(sha256sum) -c
	$(unzip) -o $(ast_grep_dir)/darwin-arm64/archive.zip -d $(ast_grep_dir)/darwin-arm64
	rm $(ast_grep_dir)/darwin-arm64/archive.zip
	touch $@

$(ast_grep_dir)/darwin-arm64/VERSION: $(ast_grep_dir)/darwin-arm64/.extracted
	echo "$(ast_grep_version)" > $@

$(ast_grep_dir)/darwin-arm64/SHA: $(ast_grep_dir)/darwin-arm64/.extracted
	echo "$(ast_grep_darwin_arm64_sha)" | head -c 8 > $@

$(ast_grep_dir)/linux-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(ast_grep_dir)/linux-arm64 \
	rw:/dev/null
$(ast_grep_dir)/linux-arm64/.extracted: | $(ast_grep_dir)/linux-arm64
	$(curl) -o $(ast_grep_dir)/linux-arm64/archive.zip $(ast_grep_linux_arm64_url)
	cd $(ast_grep_dir)/linux-arm64 && echo "$(ast_grep_linux_arm64_sha)  archive.zip" | $(sha256sum) -c
	$(unzip) -o $(ast_grep_dir)/linux-arm64/archive.zip -d $(ast_grep_dir)/linux-arm64
	rm $(ast_grep_dir)/linux-arm64/archive.zip
	touch $@

$(ast_grep_dir)/linux-arm64/VERSION: $(ast_grep_dir)/linux-arm64/.extracted
	echo "$(ast_grep_version)" > $@

$(ast_grep_dir)/linux-arm64/SHA: $(ast_grep_dir)/linux-arm64/.extracted
	echo "$(ast_grep_linux_arm64_sha)" | head -c 8 > $@

$(ast_grep_dir)/linux-x86_64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(ast_grep_dir)/linux-x86_64 \
	rw:/dev/null
$(ast_grep_dir)/linux-x86_64/.extracted: | $(ast_grep_dir)/linux-x86_64
	$(curl) -o $(ast_grep_dir)/linux-x86_64/archive.zip $(ast_grep_linux_x86_64_url)
	cd $(ast_grep_dir)/linux-x86_64 && echo "$(ast_grep_linux_x86_64_sha)  archive.zip" | $(sha256sum) -c
	$(unzip) -o $(ast_grep_dir)/linux-x86_64/archive.zip -d $(ast_grep_dir)/linux-x86_64
	rm $(ast_grep_dir)/linux-x86_64/archive.zip
	touch $@

$(ast_grep_dir)/linux-x86_64/VERSION: $(ast_grep_dir)/linux-x86_64/.extracted
	echo "$(ast_grep_version)" > $@

$(ast_grep_dir)/linux-x86_64/SHA: $(ast_grep_dir)/linux-x86_64/.extracted
	echo "$(ast_grep_linux_x86_64_sha)" | head -c 8 > $@

$(ast_grep_dir)/darwin-arm64:
	mkdir -p $@

$(ast_grep_dir)/linux-arm64:
	mkdir -p $@

$(ast_grep_dir)/linux-x86_64:
	mkdir -p $@

ast_grep_binaries := \
	$(ast_grep_dir)/darwin-arm64/.extracted \
	$(ast_grep_dir)/darwin-arm64/VERSION \
	$(ast_grep_dir)/darwin-arm64/SHA \
	$(ast_grep_dir)/linux-arm64/.extracted \
	$(ast_grep_dir)/linux-arm64/VERSION \
	$(ast_grep_dir)/linux-arm64/SHA \
	$(ast_grep_dir)/linux-x86_64/.extracted \
	$(ast_grep_dir)/linux-x86_64/VERSION \
	$(ast_grep_dir)/linux-x86_64/SHA
