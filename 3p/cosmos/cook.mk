# cosmos binaries from whilp/cosmopolitan fork
cosmos_dir := $(3p)/cosmos
cosmos_version := 2025.12.26-a673fc43b
cosmos_url := https://github.com/whilp/cosmopolitan/releases/download/$(cosmos_version)

$(eval $(call download_binary_rule,cosmos,lua,$(cosmos_url)/lua,9867c3c591733f722167e1a302719d0f5e27bd0f2aa682386d87271c365fc7d9))
$(eval $(call download_binary_rule,cosmos,zip,$(cosmos_url)/zip,10592c9954e4cae42600dffe6c2723ac5ec1dd74aec73dea1bd5c481fc930155))
$(eval $(call download_binary_rule,cosmos,unzip,$(cosmos_url)/unzip,52757dcadd5a9dcca5e390222a7081e3b037dd58560a9c5a2583b8bd0ebd9f46))
$(eval $(call download_binary_rule,cosmos,make,$(cosmos_url)/make,9486b2f97e9c10dd6641fec92eccf87a9ba2aeadf6a8162525d2d92015db1a53))

cosmos_bin := $(cosmos_make_bin)

$(cosmos_dir)/bin:
	mkdir -p $@
