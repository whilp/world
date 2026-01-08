modules += build
build_srcs := $(wildcard lib/build/*.lua)
build_fetch := $(o)/bin/build-fetch.lua
build_stage := $(o)/bin/build-stage.lua
build_check_update := $(o)/bin/check-update.lua
build_reporter := $(o)/bin/reporter.lua
build_help := $(o)/bin/make-help.lua
build_files := $(build_fetch) $(build_stage) $(build_check_update) $(build_reporter) $(build_help)
build_tests := $(wildcard lib/build/test_*.lua)
build_snaps := $(wildcard lib/build/*.snap)

.PRECIOUS: $(build_files)
reporter := $(bootstrap_cosmic) -- $(build_reporter)
update_runner := $(bootstrap_cosmic) -- $(build_check_update)

# test_reporter needs cosmic binary and checker module
$(o)/lib/build/test_reporter.lua.test.ok: $$(cosmic_bin) $$(checker_files)

# make-help snapshot: generate actual help output
$(o)/lib/build/make-help.snap: Makefile $(build_help)
	@mkdir -p $(@D)
	@$(bootstrap_cosmic) $(build_help) Makefile > $@

# Snapshot test pattern: compare actual vs expected
$(o)/lib/build/%.snap.test.ok: lib/build/%.snap $(o)/lib/build/%.snap
	@mkdir -p $(@D)
	@if diff -u $< $(word 2,$^) > $@.diff 2>&1; then \
		echo "pass" > $@; \
		echo "" >> $@; \
		echo "## stdout" >> $@; \
		echo "" >> $@; \
		echo "## stderr" >> $@; \
	else \
		echo "fail: snapshot mismatch" > $@; \
		echo "" >> $@; \
		echo "## stdout" >> $@; \
		echo "" >> $@; \
		echo "## stderr" >> $@; \
		cat $@.diff >> $@; \
		exit 1; \
	fi

# Add snapshot tests to build tests
build_tests += $(build_snaps)
