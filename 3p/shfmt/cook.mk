shfmt_dir := $(3p)/shfmt

shfmt_darwin_arm64_url := https://github.com/mvdan/sh/releases/download/v3.10.0/shfmt_v3.10.0_darwin_arm64
shfmt_darwin_arm64_sha := 86030533a823c0a7cd92dee0f74094e5b901c3277b43def6337d5e19e56fe553

shfmt_linux_arm64_url := https://github.com/mvdan/sh/releases/download/v3.10.0/shfmt_v3.10.0_linux_arm64
shfmt_linux_arm64_sha := 9d23013d56640e228732fd2a04a9ede0ab46bc2d764bf22a4a35fb1b14d707a8

shfmt_linux_x86_64_url := https://github.com/mvdan/sh/releases/download/v3.10.0/shfmt_v3.10.0_linux_amd64
shfmt_linux_x86_64_sha := 1f57a384d59542f8fac5f503da1f3ea44242f46dff969569e80b524d64b71dbc

$(shfmt_dir)/darwin-arm64/.extracted: | $(shfmt_dir)/darwin-arm64
	$(curl) -o $(shfmt_dir)/darwin-arm64/shfmt $(shfmt_darwin_arm64_url)
	cd $(shfmt_dir)/darwin-arm64 && echo "$(shfmt_darwin_arm64_sha)  shfmt" | $(sha256sum) -c
	chmod +x $(shfmt_dir)/darwin-arm64/shfmt
	touch $@

$(shfmt_dir)/linux-arm64/.extracted: | $(shfmt_dir)/linux-arm64
	$(curl) -o $(shfmt_dir)/linux-arm64/shfmt $(shfmt_linux_arm64_url)
	cd $(shfmt_dir)/linux-arm64 && echo "$(shfmt_linux_arm64_sha)  shfmt" | $(sha256sum) -c
	chmod +x $(shfmt_dir)/linux-arm64/shfmt
	touch $@

$(shfmt_dir)/linux-x86_64/.extracted: | $(shfmt_dir)/linux-x86_64
	$(curl) -o $(shfmt_dir)/linux-x86_64/shfmt $(shfmt_linux_x86_64_url)
	cd $(shfmt_dir)/linux-x86_64 && echo "$(shfmt_linux_x86_64_sha)  shfmt" | $(sha256sum) -c
	chmod +x $(shfmt_dir)/linux-x86_64/shfmt
	touch $@

$(shfmt_dir)/darwin-arm64:
	mkdir -p $@

$(shfmt_dir)/linux-arm64:
	mkdir -p $@

$(shfmt_dir)/linux-x86_64:
	mkdir -p $@

shfmt_binaries := \
	$(shfmt_dir)/darwin-arm64/.extracted \
	$(shfmt_dir)/linux-arm64/.extracted \
	$(shfmt_dir)/linux-x86_64/.extracted
