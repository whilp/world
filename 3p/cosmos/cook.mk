# cosmos binaries from whilp/cosmopolitan fork
cosmos_dir := $(3p)/cosmos
cosmos_version := 2025.12.28-2534b5d7c
cosmos_url := https://github.com/whilp/cosmopolitan/releases/download/$(cosmos_version)

$(eval $(call download_binary_rule,cosmos,lua,$(cosmos_url)/lua,8e9bd8a80382bf949b3465b716e9033962972ef8e2d16163432b1bc7557668d5))
$(eval $(call download_binary_rule,cosmos,zip,$(cosmos_url)/zip,357ae4715d03e7eecfaf65645b8f5e7507f02258fb4296670e17776ffc39c380))
$(eval $(call download_binary_rule,cosmos,unzip,$(cosmos_url)/unzip,54b5aa8b3ef97c4360430dd1325325c17745f3cbbc34740c2c6f02a779991f31))
$(eval $(call download_binary_rule,cosmos,make,$(cosmos_url)/make,3804b0ed4dc50b10acdac766280d71fe066b349f456e10eb59cca4fbb456b569))

cosmos_bin := $(cosmos_make_bin)

cosmos: $(cosmos_lua_bin) $(cosmos_zip_bin) $(cosmos_unzip_bin) $(cosmos_make_bin)
.PHONY: cosmos

$(cosmos_dir)/bin:
	mkdir -p $@
