modules += build
build_srcs := $(wildcard lib/build/*.lua)
build_sandbox := $(o)/bin/sandbox.lua
build_fetch_script := $(o)/bin/build-fetch.lua
build_stage_script := $(o)/bin/build-stage.lua
build_check_update := $(o)/bin/check-update.lua
build_reporter := $(o)/bin/reporter.lua
build_help := $(o)/bin/make-help.lua
build_snap := $(o)/bin/test-snap.lua
build_files := $(build_sandbox) $(build_fetch_script) $(build_stage_script) $(build_check_update) $(build_reporter) $(build_help) $(build_snap)
build_tests := $(wildcard lib/build/test_*.lua)
build_snaps := $(wildcard lib/build/*.snap)

.PRECIOUS: $(build_files)

# sandboxed build commands
build_fetch := $(bootstrap_cosmic) $(build_sandbox) fetch $(build_fetch_script)
build_stage := $(bootstrap_cosmic) $(build_sandbox) stage $(build_stage_script)
reporter := $(bootstrap_cosmic) -- $(build_reporter)
update_runner := $(bootstrap_cosmic) -- $(build_check_update)

# test_reporter needs cosmic binary and checker module
$(o)/lib/build/test_reporter.lua.test.ok: $$(cosmic_bin) $$(checker_files)

# make-help snapshot: generate actual help output
$(o)/lib/build/make-help.snap: Makefile $(build_help)
	@mkdir -p $(@D)
	@$(bootstrap_cosmic) $(build_help) Makefile > $@
