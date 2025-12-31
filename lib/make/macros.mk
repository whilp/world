# common macros for makefile simplification
# uses landlock-make features: .UNVEIL, .PLEDGE, .CPU, .MEMORY, .NPROC

# resource limits for safety
.CPU = 300
.MEMORY = 4G
.NPROC = 64

# platform_binaries_zip_rule: generate a binaries zip for a platform
# $(1) = platform (e.g. darwin-arm64)
define platform_binaries_zip_rule
results/binaries-$(1).zip: private .UNVEIL = r:$(o)/$(1) rx:$$(cosmos_zip_bin) rwc:results rw:/dev/null
results/binaries-$(1).zip: private .PLEDGE = stdio rpath wpath cpath fattr exec proc
results/binaries-$(1).zip: $$(all_binaries) $$(cosmos_zip_bin) | results
	cd $(o)/$(1)/3p && \
		find . -type f ! -name '.extracted' ! -name '.bundled' | \
		$$(cosmos_zip_bin) -q $$(CURDIR)/$$@ -@
endef
