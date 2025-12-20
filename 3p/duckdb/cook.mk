duckdb_dir := $(3p)/duckdb

duckdb_darwin_arm64_url := https://github.com/duckdb/duckdb/releases/download/v1.4.2/duckdb_cli-osx-arm64.zip
duckdb_darwin_arm64_sha := 4dda25dff89b9757dd248f3a48c4d3e215dff64c4c9535a7822b3b7a7f4031c2

duckdb_linux_arm64_url := https://github.com/duckdb/duckdb/releases/download/v1.4.2/duckdb_cli-linux-arm64.zip
duckdb_linux_arm64_sha := 2b62c2fa4cb2f2e76e937b3b4baf20259cf6a5370e07ff310008ca9d5d6009c4

duckdb_linux_x86_64_url := https://github.com/duckdb/duckdb/releases/download/v1.4.2/duckdb_cli-linux-amd64.zip
duckdb_linux_x86_64_sha := fae3ba93eedf20b08bca4b23aeac1ba94c446f1c10d029c193e2fc4b4e0bc1bc

$(duckdb_dir)/darwin-arm64/.extracted: | $(duckdb_dir)/darwin-arm64
	$(curl) -o $(duckdb_dir)/darwin-arm64/archive.zip $(duckdb_darwin_arm64_url)
	cd $(duckdb_dir)/darwin-arm64 && echo "$(duckdb_darwin_arm64_sha)  archive.zip" | $(sha256sum) -c
	$(unzip) -o $(duckdb_dir)/darwin-arm64/archive.zip -d $(duckdb_dir)/darwin-arm64
	rm $(duckdb_dir)/darwin-arm64/archive.zip
	touch $@

$(duckdb_dir)/linux-arm64/.extracted: | $(duckdb_dir)/linux-arm64
	$(curl) -o $(duckdb_dir)/linux-arm64/archive.zip $(duckdb_linux_arm64_url)
	cd $(duckdb_dir)/linux-arm64 && echo "$(duckdb_linux_arm64_sha)  archive.zip" | $(sha256sum) -c
	$(unzip) -o $(duckdb_dir)/linux-arm64/archive.zip -d $(duckdb_dir)/linux-arm64
	rm $(duckdb_dir)/linux-arm64/archive.zip
	touch $@

$(duckdb_dir)/linux-x86_64/.extracted: | $(duckdb_dir)/linux-x86_64
	$(curl) -o $(duckdb_dir)/linux-x86_64/archive.zip $(duckdb_linux_x86_64_url)
	cd $(duckdb_dir)/linux-x86_64 && echo "$(duckdb_linux_x86_64_sha)  archive.zip" | $(sha256sum) -c
	$(unzip) -o $(duckdb_dir)/linux-x86_64/archive.zip -d $(duckdb_dir)/linux-x86_64
	rm $(duckdb_dir)/linux-x86_64/archive.zip
	touch $@

$(duckdb_dir)/darwin-arm64:
	mkdir -p $@

$(duckdb_dir)/linux-arm64:
	mkdir -p $@

$(duckdb_dir)/linux-x86_64:
	mkdir -p $@

duckdb_binaries := \
	$(duckdb_dir)/darwin-arm64/.extracted \
	$(duckdb_dir)/linux-arm64/.extracted \
	$(duckdb_dir)/linux-x86_64/.extracted
