modules += bun
bun_version := 3p/bun/version.lua
bun_tests := 3p/bun/test_bun.tl
bun_tl_files := 3p/bun/run-bun.tl

bun_check := $(o)/3p/bun/run-bun.lua
bun_checker := $(bootstrap_cosmic) -- $(bun_check)

# bun syntax check for .gs/.js files
$(o)/%.bun.ok: % $(bun_check) $(checker_files) $$(bun_staged) | $(bootstrap_files)
	@mkdir -p $(@D)
	@BUN_BIN=$(bun_dir) $(bun_checker) $< > $@
