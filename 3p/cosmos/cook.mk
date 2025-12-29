# cosmos binaries from whilp/cosmopolitan fork
cosmos_dir := $(3p)/cosmos
cosmos_version := 2025.12.29-6868479b1
cosmos_url := https://github.com/whilp/cosmopolitan/releases/download/$(cosmos_version)

$(eval $(call download_binary_rule,cosmos,lua,$(cosmos_url)/lua,52e92da125637916adedfa2fd7651d8430134fec63671ce7d6d2e636c73bc2a4))
$(eval $(call download_binary_rule,cosmos,zip,$(cosmos_url)/zip,0dc386094952aa9f19e1de2fe255330dd039a14bd348bb951ba4a06e072807fe))
$(eval $(call download_binary_rule,cosmos,unzip,$(cosmos_url)/unzip,65538efad171d952de6c4e9c99b18ad11e293ab38f546fd526d76e2892f68399))
$(eval $(call download_binary_rule,cosmos,make,$(cosmos_url)/make,c7c8e7f09a1ed51d875bd6b3b1048e9faeb9d76cb3c7eedf01e153cced4a9373))

cosmos_bin := $(cosmos_make_bin)

cosmos: $(cosmos_lua_bin) $(cosmos_zip_bin) $(cosmos_unzip_bin) $(cosmos_make_bin)
.PHONY: cosmos

$(cosmos_dir)/bin:
	mkdir -p $@
