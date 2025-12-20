marksman_dir := $(3p)/marksman

marksman_darwin_arm64_url := https://github.com/artempyanykh/marksman/releases/download/2024-12-18/marksman-macos
marksman_darwin_arm64_sha := 7e18803966231a33ee107d0d26f69b41f2f0dc1332c52dd9729c2e29fb77be83

marksman_linux_arm64_url := https://github.com/artempyanykh/marksman/releases/download/2024-12-18/marksman-linux-arm64
marksman_linux_arm64_sha := b8d6972a56f3f9b7bbbf7c77ef8998e3b66fa82fb03c01398e224144486c9e73

marksman_linux_x86_64_url := https://github.com/artempyanykh/marksman/releases/download/2024-12-18/marksman-linux-x64
marksman_linux_x86_64_sha := b9cb666c643dfd9b699811fdfc445ed4c56be65c1d878c21d46847f0d7b0e475

$(marksman_dir)/darwin-arm64/.extracted: | $(marksman_dir)/darwin-arm64
	$(curl) -o $(marksman_dir)/darwin-arm64/marksman $(marksman_darwin_arm64_url)
	cd $(marksman_dir)/darwin-arm64 && echo "$(marksman_darwin_arm64_sha)  marksman" | $(sha256sum) -c
	chmod +x $(marksman_dir)/darwin-arm64/marksman
	touch $@

$(marksman_dir)/linux-arm64/.extracted: | $(marksman_dir)/linux-arm64
	$(curl) -o $(marksman_dir)/linux-arm64/marksman $(marksman_linux_arm64_url)
	cd $(marksman_dir)/linux-arm64 && echo "$(marksman_linux_arm64_sha)  marksman" | $(sha256sum) -c
	chmod +x $(marksman_dir)/linux-arm64/marksman
	touch $@

$(marksman_dir)/linux-x86_64/.extracted: | $(marksman_dir)/linux-x86_64
	$(curl) -o $(marksman_dir)/linux-x86_64/marksman $(marksman_linux_x86_64_url)
	cd $(marksman_dir)/linux-x86_64 && echo "$(marksman_linux_x86_64_sha)  marksman" | $(sha256sum) -c
	chmod +x $(marksman_dir)/linux-x86_64/marksman
	touch $@

$(marksman_dir)/darwin-arm64:
	mkdir -p $@

$(marksman_dir)/linux-arm64:
	mkdir -p $@

$(marksman_dir)/linux-x86_64:
	mkdir -p $@

marksman_binaries := \
	$(marksman_dir)/darwin-arm64/.extracted \
	$(marksman_dir)/linux-arm64/.extracted \
	$(marksman_dir)/linux-x86_64/.extracted
