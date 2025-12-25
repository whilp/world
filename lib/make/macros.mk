# common macros for makefile simplification
# uses landlock-make features: .UNVEIL, .PLEDGE, .CPU, .MEMORY, .NPROC

# resource limits for safety
.CPU = 300
.MEMORY = 4G
.NPROC = 64

# download_zip_rule: generate rules for downloading and extracting a zip archive
# $(1) = name (e.g. cosmocc)
# $(2) = url
# $(3) = sha256
# $(4) = target binary path relative to extract dir (e.g. bin/cosmocc)
define download_zip_rule
$(1)_dir := $$(3p)/$(1)
$(1)_zip := $$($(1)_dir)/$(1).zip
$(1)_bin := $$($(1)_dir)/$(4)

$$($(1)_bin): private .UNVEIL = r:$$($(1)_dir)/$(1).zip rwc:$$($(1)_dir) rw:/dev/null
$$($(1)_bin): private .PLEDGE = stdio rpath wpath cpath fattr
$$($(1)_bin): $$($(1)_zip)
	$$(unzip) -o $$< -d $$($(1)_dir)

$$($(1)_zip): private .UNVEIL = r:/etc/resolv.conf r:/etc/ssl rwc:$$($(1)_dir) rw:/dev/null
$$($(1)_zip): private .PLEDGE = stdio rpath wpath cpath inet dns
$$($(1)_zip): private .INTERNET = 1
$$($(1)_zip): | $$($(1)_dir)
	$$(curl) -o $$@ $(2)
	cd $$(dir $$@) && echo "$(3)  $$(notdir $$@)" | $$(sha256sum) -c

$$($(1)_dir):
	mkdir -p $$@
endef

# download_tarball_rule: generate rules for downloading and extracting a tarball
# $(1) = name (e.g. cosmopolitan)
# $(2) = url
# $(3) = sha256
# $(4) = extracted dir name (e.g. cosmopolitan-4.0.2)
# $(5) = marker file relative to extracted dir (e.g. Makefile)
define download_tarball_rule
$(1)_dir := $$(3p)/$(1)
$(1)_tarball := $$($(1)_dir)/$(1).tar.gz
$(1)_src := $$($(1)_dir)/$(4)/$(5)

$$($(1)_src): private .UNVEIL = r:$$($(1)_dir)/$(1).tar.gz rwc:$$($(1)_dir) rw:/dev/null
$$($(1)_src): private .PLEDGE = stdio rpath wpath cpath fattr
$$($(1)_src): $$($(1)_tarball)
	cd $$($(1)_dir) && $$(tar) -xzf $$(notdir $$<)

$$($(1)_tarball): private .UNVEIL = r:/etc/resolv.conf r:/etc/ssl rwc:$$($(1)_dir) rw:/dev/null
$$($(1)_tarball): private .PLEDGE = stdio rpath wpath cpath inet dns
$$($(1)_tarball): private .INTERNET = 1
$$($(1)_tarball): | $$($(1)_dir)
	$$(curl) -o $$@ $(2)
	cd $$(dir $$@) && echo "$(3)  $$(notdir $$@)" | $$(sha256sum) -c

$$($(1)_dir):
	mkdir -p $$@
endef

# platform_binaries_zip_rule: generate a binaries zip for a platform
# $(1) = platform (e.g. darwin-arm64)
define platform_binaries_zip_rule
results/binaries-$(1).zip: private .UNVEIL = r:$$(3p) rx:$$(cosmos_zip_bin) rwc:results rw:/dev/null
results/binaries-$(1).zip: private .PLEDGE = stdio rpath wpath cpath fattr exec proc
results/binaries-$(1).zip: $$(all_binaries) $$(cosmos_zip_bin) | results
	cd $$(3p) && \
		find . -path '*/$(1)/*' -type f ! -name '.extracted' | \
		$$(cosmos_zip_bin) -q $$(CURDIR)/$$@ -@
endef
