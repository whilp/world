ast_grep_dir := $(3p)/ast-grep

ast_grep_darwin_arm64_url := https://github.com/ast-grep/ast-grep/releases/download/0.28.0/app-aarch64-apple-darwin.zip
ast_grep_darwin_arm64_sha := c9a9e690d94cd9696d2552690fe0abdd2c303e48a3ee5cf9d38728eda054f147

ast_grep_linux_arm64_url := https://github.com/ast-grep/ast-grep/releases/download/0.28.0/app-aarch64-unknown-linux-gnu.zip
ast_grep_linux_arm64_sha := 62e9e79148be33d27fde24f4dcda83eab207a297ce50fb4a0becfbb29c8f218b

ast_grep_linux_x86_64_url := https://github.com/ast-grep/ast-grep/releases/download/0.28.0/app-x86_64-unknown-linux-gnu.zip
ast_grep_linux_x86_64_sha := d28be5970afb3e8022210fb9427de0875f1d64f4e4b91ed28b3a3abfebb1d934

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

$(ast_grep_dir)/darwin-arm64:
	mkdir -p $@

$(ast_grep_dir)/linux-arm64:
	mkdir -p $@

$(ast_grep_dir)/linux-x86_64:
	mkdir -p $@

ast_grep_binaries := \
	$(ast_grep_dir)/darwin-arm64/.extracted \
	$(ast_grep_dir)/linux-arm64/.extracted \
	$(ast_grep_dir)/linux-x86_64/.extracted
