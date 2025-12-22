tree_sitter_dir := $(3p)/tree-sitter

tree_sitter_version := 0.25.8

tree_sitter_darwin_arm64_url := https://github.com/tree-sitter/tree-sitter/releases/download/v$(tree_sitter_version)/tree-sitter-macos-arm64.gz
tree_sitter_darwin_arm64_sha := ae3bbba3ba68e759a949e7591a42100a12d660cae165837aba48cae76a599e64

tree_sitter_linux_arm64_url := https://github.com/tree-sitter/tree-sitter/releases/download/v$(tree_sitter_version)/tree-sitter-linux-arm64.gz
tree_sitter_linux_arm64_sha := cd81d0108df9bdacf4fd32ec53534acced4780540eb5e889c77470d496e37fc5

tree_sitter_linux_x86_64_url := https://github.com/tree-sitter/tree-sitter/releases/download/v$(tree_sitter_version)/tree-sitter-linux-x64.gz
tree_sitter_linux_x86_64_sha := c9d46697e3e5ae6900a39ad4483667d2ba14c8ffb12c3f863bcf82a9564ee19f

$(tree_sitter_dir)/darwin-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(tree_sitter_dir)/darwin-arm64 \
	rw:/dev/null
$(tree_sitter_dir)/darwin-arm64/.extracted: | $(tree_sitter_dir)/darwin-arm64
	$(curl) -o $(tree_sitter_dir)/darwin-arm64/tree-sitter.gz $(tree_sitter_darwin_arm64_url)
	cd $(tree_sitter_dir)/darwin-arm64 && echo "$(tree_sitter_darwin_arm64_sha)  tree-sitter.gz" | $(sha256sum) -c
	gunzip -f $(tree_sitter_dir)/darwin-arm64/tree-sitter.gz
	chmod +x $(tree_sitter_dir)/darwin-arm64/tree-sitter
	touch $@

$(tree_sitter_dir)/darwin-arm64/VERSION: $(tree_sitter_dir)/darwin-arm64/.extracted
	echo "$(tree_sitter_version)" > $@

$(tree_sitter_dir)/darwin-arm64/SHA: $(tree_sitter_dir)/darwin-arm64/.extracted
	echo "$(tree_sitter_darwin_arm64_sha)" | head -c 8 > $@

$(tree_sitter_dir)/linux-arm64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(tree_sitter_dir)/linux-arm64 \
	rw:/dev/null
$(tree_sitter_dir)/linux-arm64/.extracted: | $(tree_sitter_dir)/linux-arm64
	$(curl) -o $(tree_sitter_dir)/linux-arm64/tree-sitter.gz $(tree_sitter_linux_arm64_url)
	cd $(tree_sitter_dir)/linux-arm64 && echo "$(tree_sitter_linux_arm64_sha)  tree-sitter.gz" | $(sha256sum) -c
	gunzip -f $(tree_sitter_dir)/linux-arm64/tree-sitter.gz
	chmod +x $(tree_sitter_dir)/linux-arm64/tree-sitter
	touch $@

$(tree_sitter_dir)/linux-arm64/VERSION: $(tree_sitter_dir)/linux-arm64/.extracted
	echo "$(tree_sitter_version)" > $@

$(tree_sitter_dir)/linux-arm64/SHA: $(tree_sitter_dir)/linux-arm64/.extracted
	echo "$(tree_sitter_linux_arm64_sha)" | head -c 8 > $@

$(tree_sitter_dir)/linux-x86_64/.extracted: private .UNVEIL = \
	r:/etc/resolv.conf \
	r:/etc/ssl \
	rwc:$(tree_sitter_dir)/linux-x86_64 \
	rw:/dev/null
$(tree_sitter_dir)/linux-x86_64/.extracted: | $(tree_sitter_dir)/linux-x86_64
	$(curl) -o $(tree_sitter_dir)/linux-x86_64/tree-sitter.gz $(tree_sitter_linux_x86_64_url)
	cd $(tree_sitter_dir)/linux-x86_64 && echo "$(tree_sitter_linux_x86_64_sha)  tree-sitter.gz" | $(sha256sum) -c
	gunzip -f $(tree_sitter_dir)/linux-x86_64/tree-sitter.gz
	chmod +x $(tree_sitter_dir)/linux-x86_64/tree-sitter
	touch $@

$(tree_sitter_dir)/linux-x86_64/VERSION: $(tree_sitter_dir)/linux-x86_64/.extracted
	echo "$(tree_sitter_version)" > $@

$(tree_sitter_dir)/linux-x86_64/SHA: $(tree_sitter_dir)/linux-x86_64/.extracted
	echo "$(tree_sitter_linux_x86_64_sha)" | head -c 8 > $@

$(tree_sitter_dir)/darwin-arm64:
	mkdir -p $@

$(tree_sitter_dir)/linux-arm64:
	mkdir -p $@

$(tree_sitter_dir)/linux-x86_64:
	mkdir -p $@

tree_sitter_binaries := \
	$(tree_sitter_dir)/darwin-arm64/.extracted \
	$(tree_sitter_dir)/darwin-arm64/VERSION \
	$(tree_sitter_dir)/darwin-arm64/SHA \
	$(tree_sitter_dir)/linux-arm64/.extracted \
	$(tree_sitter_dir)/linux-arm64/VERSION \
	$(tree_sitter_dir)/linux-arm64/SHA \
	$(tree_sitter_dir)/linux-x86_64/.extracted \
	$(tree_sitter_dir)/linux-x86_64/VERSION \
	$(tree_sitter_dir)/linux-x86_64/SHA
