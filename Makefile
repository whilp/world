.SECONDEXPANSION:

# auto-parallelize based on available CPUs
nproc := $(shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)
MAKEFLAGS += -j$(nproc)

modules :=
o := o

export PATH := $(CURDIR)/$(o)/bin:$(PATH)
export STAGE_O := $(CURDIR)/$(o)/staged
export FETCH_O := $(CURDIR)/$(o)/fetched

uname_s := $(shell uname -s)
uname_m := $(shell uname -m)
os := $(if $(filter Darwin,$(uname_s)),darwin,linux)
arch := $(subst aarch64,arm64,$(uname_m))
platforms := darwin-arm64 linux-arm64 linux-x86_64
platform := $(os)-$(arch)

include bootstrap/cook.mk
include lib/build/cook.mk
include lib/test/cook.mk
include 3p/luaunit/cook.mk
include 3p/ast-grep/cook.mk
include 3p/cosmos/cook.mk
include 3p/argparse/cook.mk
include 3p/rg/cook.mk
include 3p/gh/cook.mk
include 3p/uv/cook.mk
include 3p/shfmt/cook.mk
include 3p/stylua/cook.mk
include 3p/biome/cook.mk
include 3p/comrak/cook.mk
include 3p/delta/cook.mk
include 3p/ruff/cook.mk
include 3p/sqruff/cook.mk
include 3p/superhtml/cook.mk
include 3p/marksman/cook.mk
include 3p/duckdb/cook.mk
include 3p/tree-sitter/cook.mk
include 3p/nvim-conform/cook.mk
include 3p/nvim-mini/cook.mk
include 3p/nvim-lspconfig/cook.mk
include 3p/nvim-treesitter/cook.mk
include 3p/nvim-parsers/cook.mk
include 3p/nvim/cook.mk
include 3p/luacheck/cook.mk
include 3p/tl/cook.mk
include lib/skill/cook.mk
include lib/cosmic/cook.mk
include lib/home/cook.mk
include lib/checker/cook.mk

include cook.mk

# srcs are copied to o/
all_files := $(foreach x,$(modules),$(addprefix $(o)/$(x)/,$($(x)_srcs)))

cp := cp -p

$(o)/%: %
	@mkdir -p $(@D)
	@$(cp) $< $@

# bin scripts: o/bin/X.lua from lib/*/X.lua and 3p/*/X.lua
vpath %.lua lib/build lib/test 3p/ast-grep 3p/luacheck 3p/tl
$(o)/bin/%.lua: %.lua
	@mkdir -p $(@D)
	@$(cp) $< $@

# files are produced in o/
all_files += $(foreach x,$(modules),$($(x)_files))

# define *_staged, *_dir for versioned modules (must be before dep expansion)
# modules can override *_dir for post-processing (e.g., nvim bundles plugins)
$(foreach m,$(modules),$(if $($(m)_version),\
  $(eval $(m)_staged := $(o)/$(m)/.staged)\
  $(if $($(m)_dir),,$(eval $(m)_dir := $(o)/$(m)/.staged))))

# default deps for regular modules (also excluded from file dep expansion)
default_deps := bootstrap test

# expand module deps: M_files depends on deps' _files and _staged
$(foreach m,$(filter-out $(default_deps),$(modules)),\
  $(foreach d,$($(m)_deps),\
    $(eval $($(m)_files): $($(d)_files))\
    $(if $($(d)_staged),\
      $(eval $($(m)_files): $($(d)_staged)))))

all_versions := $(foreach x,$(modules),$($(x)_version))

# versioned modules: o/module/.versioned -> version.lua
$(foreach m,$(modules),$(if $($(m)_version),\
  $(eval $(o)/$(m)/.versioned: $($(m)_version) ; @mkdir -p $$(@D) && ln -sfn $(CURDIR)/$$< $$@)))
all_versioned := $(foreach m,$(modules),$(if $($(m)_version),$(o)/$(m)/.versioned))

# versions get fetched: o/module/.fetched -> o/fetched/module/<ver>-<sha>/<archive>
.PHONY: fetched
all_fetched := $(patsubst %/.versioned,%/.fetched,$(all_versioned))
fetched: $(all_fetched)
$(o)/%/.fetched: $(o)/%/.versioned $(build_files) | $(bootstrap_cosmic)
	@$(build_fetch) $$(readlink $<) $(platform) $@

# versions get staged: o/module/.staged -> o/staged/module/<ver>-<sha>
.PHONY: staged
all_staged := $(patsubst %/.fetched,%/.staged,$(all_fetched))
staged: $(all_staged)
$(o)/%/.staged: $(o)/%/.fetched
	@$(build_stage) $$(readlink $(o)/$*/.versioned) $(platform) $< $@

all_tests := $(foreach x,$(modules),$($(x)_tests))
ifdef TEST
  # filter tests by pattern (substring match)
  all_tests := $(foreach t,$(all_tests),$(if $(findstring $(TEST),$(t)),$(t)))
endif
all_tested := $(patsubst %,o/%.tested,$(all_tests))

test: $(o)/test-summary.txt

$(o)/test-summary.txt: $(all_tested)
	@$(test_reporter) $(o) | tee $@

$(o)/test-results.txt: $(all_tested)
	@for f in $^; do echo "$${f%.tested}: $$(cat $$f)"; done > $@

export TEST_O := $(o)
export TEST_PLATFORM := $(platform)
export TEST_BIN := $(o)/bin
export LUA_PATH := $(CURDIR)/lib/?.lua;$(CURDIR)/lib/?/init.lua;;

$(o)/%.tested: % $(test_files) | $(bootstrap_files)
	@TEST_DIR=$(TEST_DIR) $< $@

# expand test deps: M's tests depend on own _files/_dir plus deps' _dir
$(foreach m,$(filter-out bootstrap,$(modules)),\
  $(eval $(patsubst %,$(o)/%.tested,$($(m)_tests)): $($(m)_files))\
  $(eval $(patsubst %,$(o)/%.tested,$($(m)_tests)): TEST_DEPS += $($(m)_files))\
  $(if $($(m)_dir),\
    $(eval $(patsubst %,$(o)/%.tested,$($(m)_tests)): $($(m)_dir))\
    $(eval $(patsubst %,$(o)/%.tested,$($(m)_tests)): TEST_DEPS += $($(m)_dir))\
    $(eval $(patsubst %,$(o)/%.tested,$($(m)_tests)): TEST_DIR := $($(m)_dir)))\
  $(foreach d,$(filter-out $(m),$(default_deps) $($(m)_deps)),\
    $(if $($(d)_dir),\
      $(eval $(patsubst %,$(o)/%.tested,$($(m)_tests)): $($(d)_dir))\
      $(eval $(patsubst %,$(o)/%.tested,$($(m)_tests)): TEST_DEPS += $($(d)_dir)))))

all_built_files := $(foreach x,$(modules),$($(x)_files))
all_test_files := $(foreach x,$(modules),$($(x)_tests))
all_version_files := $(filter-out ,$(foreach x,$(modules),$($(x)_version)))
all_checkable_files := $(all_built_files) $(addprefix $(o)/,$(all_test_files) $(all_version_files))
.PRECIOUS: $(all_checkable_files)
all_astgreps := $(patsubst %,%.astgrep.checked,$(all_checkable_files))

astgrep: $(o)/astgrep-summary.txt

$(o)/astgrep-summary.txt: $(all_astgreps)
	@$(astgrep_reporter) $(o) | tee $@

$(o)/%.astgrep.checked: $(o)/% $(ast-grep_files) | $(bootstrap_files) $(ast-grep_staged)
	@ASTGREP_BIN=$(ast-grep_staged) $(astgrep_runner) $< $@

all_luachecks := $(patsubst %,%.luacheck.checked,$(all_checkable_files))

luacheck: $(o)/luacheck-summary.txt

$(o)/luacheck-summary.txt: $(all_luachecks)
	@$(luacheck_reporter) $(o) | tee $@

$(o)/%.luacheck.checked: $(o)/% $(luacheck_files) | $(bootstrap_files) $(luacheck_staged)
	@LUACHECK_BIN=$(luacheck_staged) $(luacheck_runner) $< $@

all_teals := $(patsubst %,%.teal.checked,$(all_checkable_files))

teal: $(o)/teal-summary.txt

$(o)/teal-summary.txt: $(all_teals)
	@$(teal_reporter) $(o) | tee $@

$(o)/%.teal.checked: $(o)/% $(tl_files) | $(bootstrap_files) $(tl_staged)
	@TL_BIN=$(tl_staged) $(teal_runner) $< $@

.PHONY: bootstrap
bootstrap: $(bootstrap_files)
	@if [ -n "$$CLAUDE_ENV_FILE" ]; then \
		echo "export PATH=\"$(CURDIR)/bin:$$PATH\"" >> "$$CLAUDE_ENV_FILE"; \
	fi

.PHONY: clean
clean:
	@rm -rf $(o)

all_checks := $(all_astgreps) $(all_luachecks) $(all_teals)

check: $(o)/check-summary.txt

$(o)/check-summary.txt: $(all_checks)
	@$(check_reporter) $(o) | tee $@

# Update PR title/description from .github/pr/<number>.md
.PHONY: update-pr
update-pr: $(cosmic_bin) | $(bootstrap_cosmic)
	@if [ -f $(cosmic_bin) ]; then \
		$(cosmic_bin) -l skill update-pr || true; \
	else \
		$(bootstrap_cosmic) lib/skill/pr.lua || true; \
	fi

debug-modules:
	@echo $(modules)

