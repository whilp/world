modules += checker
checker_lua_srcs := $(wildcard lib/checker/*.lua)
checker_tl_srcs := $(wildcard lib/checker/*.tl)
checker_srcs := $(checker_lua_srcs) $(checker_tl_srcs)
checker_tests := $(filter lib/checker/test_%.tl,$(checker_tl_srcs))
# output files: .lua sources copied to o/any/, .tl sources compiled via tlconfig.lua build_dir
checker_lua_files := $(addprefix o/any/,$(filter-out $(checker_tests),$(checker_lua_srcs)))
checker_tl_files := $(patsubst lib/%.tl,o/teal/lib/%.lua,$(checker_tl_srcs))
checker_files := $(checker_lua_files) $(checker_tl_files)

# makefile checker fixtures: run make commands and capture output
checker_make_fixtures := $(wildcard lib/checker/fixtures/make/*.mk)
checker_make_out_dir := $(o)/lib/checker/fixtures/make

# generate dry-run output (captures exit code on last line)
# use trailing || true to prevent make from failing when the test makefile has errors
$(checker_make_out_dir)/%.dry-run.out: lib/checker/fixtures/make/%.mk
	@mkdir -p $(@D)
	@code=0; make -n -f $< all >$@.tmp 2>&1 || code=$$?; echo "exit:$$code" >> $@.tmp; mv $@.tmp $@

# generate warn-undefined-variables output
$(checker_make_out_dir)/%.warn-undef.out: lib/checker/fixtures/make/%.mk
	@mkdir -p $(@D)
	@code=0; make -n --warn-undefined-variables -f $< all >$@.tmp 2>&1 || code=$$?; echo "exit:$$code" >> $@.tmp; mv $@.tmp $@

# generate database output
$(checker_make_out_dir)/%.database.out: lib/checker/fixtures/make/%.mk
	@mkdir -p $(@D)
	@code=0; make -p -n -f $< >$@.tmp 2>&1 || code=$$?; echo "exit:$$code" >> $@.tmp; mv $@.tmp $@

# all make fixture outputs
checker_make_outputs := \
	$(patsubst lib/checker/fixtures/make/%.mk,$(checker_make_out_dir)/%.dry-run.out,$(checker_make_fixtures)) \
	$(patsubst lib/checker/fixtures/make/%.mk,$(checker_make_out_dir)/%.warn-undef.out,$(checker_make_fixtures)) \
	$(checker_make_out_dir)/database.database.out

# test_makefile.tl depends on generated outputs
$(o)/lib/checker/test_makefile.tl.test.ok: $(checker_make_outputs)
$(o)/lib/checker/test_makefile.tl.test.ok: TEST_DIR := $(checker_make_out_dir)
