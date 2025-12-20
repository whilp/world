comrak_dir := $(3p)/comrak

comrak_darwin_arm64_url := https://github.com/kivikakk/comrak/releases/download/v0.41.0/comrak-0.41.0-aarch64-apple-darwin
comrak_darwin_arm64_sha := ebff398559a48112e7699ad8ce8a35e1f5f0cf469ed44d55318b1d794abf1090

comrak_linux_arm64_url := https://github.com/kivikakk/comrak/releases/download/v0.41.0/comrak-0.41.0-aarch64-unknown-linux-gnu
comrak_linux_arm64_sha := b76c1a02cd2b2d2b5f9dbde9d16124aa54d9e5a66fa2bc3f5f4d0ce637b1bb64

comrak_linux_x86_64_url := https://github.com/kivikakk/comrak/releases/download/v0.41.0/comrak-0.41.0-x86_64-unknown-linux-gnu
comrak_linux_x86_64_sha := d3ffc8f04f85a47fa325081affd6b572ad456b542a4d3a1207ef4685afd7e9e2

$(comrak_dir)/darwin-arm64/.extracted: | $(comrak_dir)/darwin-arm64
	$(curl) -o $(comrak_dir)/darwin-arm64/comrak $(comrak_darwin_arm64_url)
	cd $(comrak_dir)/darwin-arm64 && echo "$(comrak_darwin_arm64_sha)  comrak" | $(sha256sum) -c
	chmod +x $(comrak_dir)/darwin-arm64/comrak
	touch $@

$(comrak_dir)/linux-arm64/.extracted: | $(comrak_dir)/linux-arm64
	$(curl) -o $(comrak_dir)/linux-arm64/comrak $(comrak_linux_arm64_url)
	cd $(comrak_dir)/linux-arm64 && echo "$(comrak_linux_arm64_sha)  comrak" | $(sha256sum) -c
	chmod +x $(comrak_dir)/linux-arm64/comrak
	touch $@

$(comrak_dir)/linux-x86_64/.extracted: | $(comrak_dir)/linux-x86_64
	$(curl) -o $(comrak_dir)/linux-x86_64/comrak $(comrak_linux_x86_64_url)
	cd $(comrak_dir)/linux-x86_64 && echo "$(comrak_linux_x86_64_sha)  comrak" | $(sha256sum) -c
	chmod +x $(comrak_dir)/linux-x86_64/comrak
	touch $@

$(comrak_dir)/darwin-arm64:
	mkdir -p $@

$(comrak_dir)/linux-arm64:
	mkdir -p $@

$(comrak_dir)/linux-x86_64:
	mkdir -p $@

comrak_binaries := \
	$(comrak_dir)/darwin-arm64/.extracted \
	$(comrak_dir)/linux-arm64/.extracted \
	$(comrak_dir)/linux-x86_64/.extracted
