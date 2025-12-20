stylua_dir := $(3p)/stylua

stylua_darwin_arm64_url := https://github.com/JohnnyMorganz/StyLua/releases/download/v2.0.1/stylua-macos-aarch64.zip
stylua_darwin_arm64_sha := 3d9caaa660da4b3bc092e805d09af59e42b7504f1253c863b682ea3fc80944f2

stylua_linux_arm64_url := https://github.com/JohnnyMorganz/StyLua/releases/download/v2.0.1/stylua-linux-aarch64.zip
stylua_linux_arm64_sha := 3db53cd00a685d0b59f4a4ab188bfa6acb804dca489d810a852ed2ea32eb2b1c

stylua_linux_x86_64_url := https://github.com/JohnnyMorganz/StyLua/releases/download/v2.0.1/stylua-linux-x86_64.zip
stylua_linux_x86_64_sha := 9087e42f599855192cf4f6a7fb0cb7353e23debd7c749c6e3a76fc58abde3c89

$(stylua_dir)/darwin-arm64/.extracted: | $(stylua_dir)/darwin-arm64
	$(curl) -o $(stylua_dir)/darwin-arm64/archive.zip $(stylua_darwin_arm64_url)
	cd $(stylua_dir)/darwin-arm64 && echo "$(stylua_darwin_arm64_sha)  archive.zip" | $(sha256sum) -c
	$(unzip) -o $(stylua_dir)/darwin-arm64/archive.zip -d $(stylua_dir)/darwin-arm64
	rm $(stylua_dir)/darwin-arm64/archive.zip
	touch $@

$(stylua_dir)/linux-arm64/.extracted: | $(stylua_dir)/linux-arm64
	$(curl) -o $(stylua_dir)/linux-arm64/archive.zip $(stylua_linux_arm64_url)
	cd $(stylua_dir)/linux-arm64 && echo "$(stylua_linux_arm64_sha)  archive.zip" | $(sha256sum) -c
	$(unzip) -o $(stylua_dir)/linux-arm64/archive.zip -d $(stylua_dir)/linux-arm64
	rm $(stylua_dir)/linux-arm64/archive.zip
	touch $@

$(stylua_dir)/linux-x86_64/.extracted: | $(stylua_dir)/linux-x86_64
	$(curl) -o $(stylua_dir)/linux-x86_64/archive.zip $(stylua_linux_x86_64_url)
	cd $(stylua_dir)/linux-x86_64 && echo "$(stylua_linux_x86_64_sha)  archive.zip" | $(sha256sum) -c
	$(unzip) -o $(stylua_dir)/linux-x86_64/archive.zip -d $(stylua_dir)/linux-x86_64
	rm $(stylua_dir)/linux-x86_64/archive.zip
	touch $@

$(stylua_dir)/darwin-arm64:
	mkdir -p $@

$(stylua_dir)/linux-arm64:
	mkdir -p $@

$(stylua_dir)/linux-x86_64:
	mkdir -p $@

stylua_binaries := \
	$(stylua_dir)/darwin-arm64/.extracted \
	$(stylua_dir)/linux-arm64/.extracted \
	$(stylua_dir)/linux-x86_64/.extracted
