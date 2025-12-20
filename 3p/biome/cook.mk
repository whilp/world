biome_dir := $(3p)/biome

biome_darwin_arm64_url := https://github.com/biomejs/biome/releases/download/cli%2Fv1.9.4/biome-darwin-arm64
biome_darwin_arm64_sha := c68f2cbe09e9485426a749353a155b1d22c130c6ccdadc7772d603eb247b9a9d

biome_linux_arm64_url := https://github.com/biomejs/biome/releases/download/cli%2Fv1.9.4/biome-linux-arm64
biome_linux_arm64_sha := f0f0f3e7cdec78420a600b05bfc364aa9b804811bd3bbae04e7bf090828ae970

biome_linux_x86_64_url := https://github.com/biomejs/biome/releases/download/cli%2Fv1.9.4/biome-linux-x64
biome_linux_x86_64_sha := ce247fb644999ef52e5111dd6fd6e471019669fc9c4a44b5699721e39b7032c3

$(biome_dir)/darwin-arm64/.extracted: | $(biome_dir)/darwin-arm64
	$(curl) -o $(biome_dir)/darwin-arm64/biome $(biome_darwin_arm64_url)
	cd $(biome_dir)/darwin-arm64 && echo "$(biome_darwin_arm64_sha)  biome" | $(sha256sum) -c
	chmod +x $(biome_dir)/darwin-arm64/biome
	touch $@

$(biome_dir)/linux-arm64/.extracted: | $(biome_dir)/linux-arm64
	$(curl) -o $(biome_dir)/linux-arm64/biome $(biome_linux_arm64_url)
	cd $(biome_dir)/linux-arm64 && echo "$(biome_linux_arm64_sha)  biome" | $(sha256sum) -c
	chmod +x $(biome_dir)/linux-arm64/biome
	touch $@

$(biome_dir)/linux-x86_64/.extracted: | $(biome_dir)/linux-x86_64
	$(curl) -o $(biome_dir)/linux-x86_64/biome $(biome_linux_x86_64_url)
	cd $(biome_dir)/linux-x86_64 && echo "$(biome_linux_x86_64_sha)  biome" | $(sha256sum) -c
	chmod +x $(biome_dir)/linux-x86_64/biome
	touch $@

$(biome_dir)/darwin-arm64:
	mkdir -p $@

$(biome_dir)/linux-arm64:
	mkdir -p $@

$(biome_dir)/linux-x86_64:
	mkdir -p $@

biome_binaries := \
	$(biome_dir)/darwin-arm64/.extracted \
	$(biome_dir)/linux-arm64/.extracted \
	$(biome_dir)/linux-x86_64/.extracted
