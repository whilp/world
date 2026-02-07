.SECONDEXPANSION:
.SECONDARY:
SHELL := /bin/bash
.SHELLFLAGS := -o pipefail -ec
.DEFAULT_GOAL := help

MAKEFLAGS += --no-print-directory
MAKEFLAGS += --no-builtin-rules
MAKEFLAGS += --no-builtin-variables
MAKEFLAGS += --output-sync

modules :=
o := o

export PATH := $(CURDIR)/$(o)/bin:$(PATH)
export STAGE_O := $(CURDIR)/$(o)/staged
export FETCH_O := $(CURDIR)/$(o)/fetched
# TL_PATH for teal type checker only (not exported globally - conflicts with cosmic teal loader)
TL_PATH := $(CURDIR)/lib/types/?.d.tl;$(CURDIR)/lib/types/?/init.d.tl;$(CURDIR)/$(o)/lib/?.tl;$(CURDIR)/$(o)/lib/?/init.tl;$(CURDIR)/$(o)/lib/home/?.tl;$(CURDIR)/$(o)/lib/home/?/init.tl;$(CURDIR)/lib/home/?.tl;$(CURDIR)/lib/home/?/init.tl;$(CURDIR)/lib/?.tl;$(CURDIR)/lib/?/init.tl

## TMP: temp directory for tests (default: /tmp, use TMP=~/tmp for more space)
TMP ?= /tmp
export TMPDIR := $(TMP)

uname_s := $(shell uname -s)
uname_m := $(shell uname -m)
os := $(if $(filter Darwin,$(uname_s)),darwin,linux)
arch := $(subst aarch64,arm64,$(uname_m))
platforms := darwin-arm64 linux-arm64 linux-x86_64
platform := $(os)-$(arch)

include bootstrap.mk
include lib/cook.mk
include 3p/ast-grep/cook.mk
include 3p/cosmos/cook.mk
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
include 3p/sprite/cook.mk
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
include 3p/cosmic/cook.mk

ifdef TEST_BUN
include 3p/bun/cook.mk
endif

ifdef TEST_CLASP
include 3p/clasp/cook.mk
endif

include cook.mk

# landlock-make sandbox constraints (only effective when using landlock-make)
# global defaults: read-only access, no network, basic stdio
.PLEDGE = stdio rpath
.UNVEIL = \
	rx:$(o)/bootstrap \
	r:lib \
	r:3p

.PHONY: help
## Show this help message
help: $(build_files) | $(bootstrap_cosmic)
	@$(bootstrap_cosmic) $(o)/bin/make-help.lua $(MAKEFILE_LIST)

## Filter targets by pattern (make test only='skill')
filter-only = $(if $(only),$(foreach f,$1,$(if $(findstring $(only),$(f)),$(f))),$1)

# srcs are copied to o/
all_files := $(call filter-only,$(foreach x,$(modules),$(addprefix $(o)/$(x)/,$($(x)_srcs))))

cp := cp -p

$(o)/%: %
	@mkdir -p $(@D)
	@$(cp) $< $@

# compile .tl files to .lua
$(o)/%.lua: %.tl $(types_files) | $(bootstrap_files)
	@mkdir -p $(@D)
	@LUA_PATH="lib/home/?.lua;lib/home/?/init.lua;$${LUA_PATH:-}" $(bootstrap_cosmic) --compile $< > $@

# bin scripts: o/bin/X.lua from lib/*/X.lua and 3p/*/X.lua
vpath %.lua lib/build lib/test 3p/ast-grep
vpath %.tl lib/build lib/test 3p/ast-grep
$(o)/bin/%.lua: %.lua
	@mkdir -p $(@D)
	@$(cp) $< $@

# bin scripts from teal: o/bin/X.lua from lib/*/X.tl (vpath finds X.tl)
$(o)/bin/%.lua: %.tl $(types_files) | $(bootstrap_files)
	@mkdir -p $(@D)
	@$(bootstrap_cosmic) --compile $< > $@

# files are produced in o/
all_files += $(call filter-only,$(foreach x,$(modules),$($(x)_files)))

# tl files: modules declare _tl_files, derive compiled .lua outputs
all_tl_files := $(call filter-only,$(foreach x,$(modules),$($(x)_tl_files)))
all_tl_lua := $(patsubst %.tl,$(o)/%.lua,$(all_tl_files))
all_files += $(all_tl_lua)

# define *_staged, *_dir for versioned modules (must be before dep expansion)
# modules can override *_dir for post-processing (e.g., nvim bundles plugins)
$(foreach m,$(modules),$(if $($(m)_version),\
  $(eval $(m)_staged := $(o)/$(m)/.staged)\
  $(if $($(m)_dir),,$(eval $(m)_dir := $(o)/$(m)/.staged))))

# define *_zip for tool modules (excludes cosmos, bootstrap infrastructure)
zip_excluded := cosmos bootstrap
$(foreach m,$(filter-out $(zip_excluded),$(modules)),$(if $($(m)_version),\
  $(eval $(m)_zip := $(o)/$(m)/.zip)))

# default deps for regular modules (also excluded from file dep expansion)
default_deps := bootstrap test

# expand module deps: M_files depends on deps' _files and _staged
$(foreach m,$(filter-out $(default_deps),$(modules)),\
  $(foreach d,$($(m)_deps),\
    $(eval $($(m)_files): $($(d)_files))\
    $(if $($(d)_staged),\
      $(eval $($(m)_files): $($(d)_staged)))))

all_versions := $(call filter-only,$(foreach x,$(modules),$($(x)_version)))
all_updated := $(patsubst %,$(o)/%.update.ok,$(all_versions))

# versioned modules: o/module/.versioned -> version.lua
$(foreach m,$(modules),$(if $($(m)_version),\
  $(eval $(o)/$(m)/.versioned: $($(m)_version) ; @mkdir -p $$(@D) && ln -sfn $(CURDIR)/$$< $$@)))
all_versioned := $(call filter-only,$(foreach m,$(modules),$(if $($(m)_version),$(o)/$(m)/.versioned)))

# versions get fetched: o/module/.fetched -> o/fetched/module/<ver>-<sha>/<archive>
.PHONY: fetched
all_fetched := $(patsubst %/.versioned,%/.fetched,$(all_versioned))
## Fetch all dependencies only
fetched: $(all_fetched)
$(o)/%/.fetched: .PLEDGE = stdio rpath wpath cpath inet dns
$(o)/%/.fetched: .UNVEIL = rx:$(o)/bootstrap r:3p rwc:$(o) r:/etc/resolv.conf r:/etc/ssl
$(o)/%/.fetched: $(o)/%/.versioned $(build_files) | $(bootstrap_cosmic)
	@$(build_fetch) $$(readlink $<) $(platform) $@

# versions get staged: o/module/.staged -> o/staged/module/<ver>-<sha>
.PHONY: staged
all_staged := $(patsubst %/.fetched,%/.staged,$(all_fetched))
## Fetch and extract all dependencies
staged: $(all_staged)
$(o)/%/.staged: .PLEDGE = stdio rpath wpath cpath proc exec
$(o)/%/.staged: .UNVEIL = rx:$(o)/bootstrap r:3p rwc:$(o) rx:/usr/bin
$(o)/%/.staged: $(o)/%/.fetched $(build_files)
	@$(build_stage) $$(readlink $(o)/$*/.versioned) $(platform) $< $@

# tool zips: o/module/.zip contains versioned dir + symlinks (for home binary)
# structure: .local/share/<tool>/<ver-sha>/* + symlinks at .local/share/<tool>/*
$(o)/%/.zip: $(o)/%/.staged $$(cosmos_staged)
	@rm -rf $(@D)/.zip-staging
	@mkdir -p $(@D)/.zip-staging/.local/share/$*
	@versioned_name=$$(basename $$(readlink -f $<)) && \
		cp -r $$(readlink -f $<) $(@D)/.zip-staging/.local/share/$*/$$versioned_name && \
		for item in $(@D)/.zip-staging/.local/share/$*/$$versioned_name/*; do \
			ln -sf $$versioned_name/$$(basename $$item) $(@D)/.zip-staging/.local/share/$*/$$(basename $$item); \
		done && \
		cd $(@D)/.zip-staging && $(CURDIR)/$(cosmos_zip) -qry $(CURDIR)/$@ .
	@rm -rf $(@D)/.zip-staging

all_tests := $(call filter-only,$(foreach x,$(modules),$($(x)_tests)))
all_release_tests := $(call filter-only,$(foreach x,$(modules),$($(x)_release_test) $($(x)_release_tests)))
all_declared_tests := $(all_tests) $(all_release_tests)
all_tested := $(patsubst %,o/%.test.ok,$(all_tests))
all_snaps := $(call filter-only,$(foreach x,$(modules),$($(x)_snaps)))
all_snapped := $(patsubst %,$(o)/%.test.ok,$(all_snaps))
all_buns := $(call filter-only,$(foreach x,$(modules),$($(x)_buns)))
all_bunned := $(patsubst %,$(o)/%.bun.ok,$(all_buns))

## Run all tests (incremental)
test: $(o)/test-summary.txt

$(o)/test-summary.txt: $(all_tested) $(all_snapped) | $(build_reporter)
	@$(reporter) --dir $(o) $^ | tee $@

export TEST_O := $(o)
export TEST_PLATFORM := $(platform)
export TEST_BIN := $(o)/bin
# Build LUA_PATH from lib_dirs (populated by cook.mk includes)
lib_dirs_lua_path := $(subst ; ,;,$(foreach d,$(lib_dirs),$(CURDIR)/$(d)/?.lua;$(CURDIR)/$(d)/?/init.lua;))
export LUA_PATH := $(CURDIR)/o/bin/?.lua;$(CURDIR)/o/teal/lib/?.lua;$(CURDIR)/o/teal/lib/?/init.lua;$(CURDIR)/o/lib/?.lua;$(CURDIR)/o/lib/?/init.lua;$(lib_dirs_lua_path)$(CURDIR)/lib/?.lua;$(CURDIR)/lib/?/init.lua;;
export NO_COLOR := 1

# Test rule: .tl tests depend on compiled .lua (Make handles compilation)
$(o)/%.tl.test.ok: .PLEDGE = stdio rpath wpath cpath proc exec
$(o)/%.tl.test.ok: .UNVEIL = rx:$(o)/bootstrap r:lib r:3p rwc:$(o) rwc:$(TMP) rx:/usr rx:/proc r:/etc r:/dev/null
$(o)/%.tl.test.ok: $(o)/%.lua $(test_files) $(checker_files) | $(bootstrap_files)
	@mkdir -p $(@D)
	@[ -x $< ] || chmod a+x $<
	-@TEST_DIR=$(TEST_DIR) $(test_runner) $< > $@

# Snapshot test pattern: compare expected vs actual
$(o)/%.snap.test.ok: .EXTRA_PREREQS = $(build_snap)
$(o)/%.snap.test.ok: %.snap $(o)/%.snap | $(bootstrap_cosmic)
	@mkdir -p $(@D)
	@$(bootstrap_cosmic) $(build_snap) $< $(word 2,$^) > $@

# expand test deps: M's tests depend on own _files/_tl_files plus deps' _dir/_files/_tl_lua
# derive compiled .lua from _tl_files (first pass: compute all _tl_lua)
$(foreach m,$(filter-out bootstrap,$(modules)),\
  $(eval $(m)_tl_lua := $(patsubst %.tl,$(o)/%.lua,$($(m)_tl_files))))
# second pass: set up test dependencies
$(foreach m,$(filter-out bootstrap,$(modules)),\
  $(eval $(patsubst %,$(o)/%.test.ok,$($(m)_tests)): $($(m)_files) $($(m)_tl_lua))\
  $(eval $(patsubst %,$(o)/%.test.ok,$($(m)_tests)): TEST_DEPS += $($(m)_files) $($(m)_tl_lua))\
  $(if $($(m)_dir),\
    $(eval $(patsubst %,$(o)/%.test.ok,$($(m)_tests)): $($(m)_dir))\
    $(eval $(patsubst %,$(o)/%.test.ok,$($(m)_tests)): TEST_DEPS += $($(m)_dir))\
    $(eval $(patsubst %,$(o)/%.test.ok,$($(m)_tests)): TEST_DIR := $($(m)_dir)))\
  $(foreach d,$(filter-out $(m),$(default_deps) $($(m)_deps)),\
    $(if $($(d)_dir),\
      $(eval $(patsubst %,$(o)/%.test.ok,$($(m)_tests)): $($(d)_dir))\
      $(eval $(patsubst %,$(o)/%.test.ok,$($(m)_tests)): TEST_DEPS += $($(d)_dir)))\
    $(eval $(patsubst %,$(o)/%.test.ok,$($(m)_tests)): $($(d)_files) $($(d)_tl_lua))))

all_built_files := $(call filter-only,$(foreach x,$(modules),$($(x)_files)))
all_built_files += $(all_tl_lua)
all_source_files := $(call filter-only,$(foreach x,$(modules),$($(x)_tests)))
all_source_files += $(call filter-only,$(filter-out ,$(foreach x,$(modules),$($(x)_version))))
all_source_files += $(call filter-only,$(foreach x,$(modules),$($(x)_srcs)))
all_source_files += $(all_tl_files)
all_checkable_files := $(addprefix $(o)/,$(all_source_files))

.PHONY: files
## Build all module files
files: $(all_built_files)

all_astgreps := $(patsubst %,%.ast-grep.ok,$(all_checkable_files))

## Run ast-grep linter on all files
astgrep: $(o)/astgrep-summary.txt

$(o)/astgrep-summary.txt: $(all_astgreps) | $(build_reporter)
	@$(reporter) --dir $(o) $^ | tee $@

$(o)/%.ast-grep.ok: $(o)/% $(ast-grep_files) $(checker_files) | $(bootstrap_files) $(ast-grep_staged)
	@mkdir -p $(@D)
	@ASTGREP_BIN=$(ast-grep_staged) $(astgrep_runner) $< > $@

all_teals := $(patsubst %,%.teal.ok,$(all_checkable_files))

## Run teal type checker on all files
teal: $(o)/teal-summary.txt

$(o)/teal-summary.txt: $(all_teals) | $(build_reporter)
	@$(reporter) --dir $(o) $^ | tee $@

# teal checker: run cosmic --check and format output for reporter
# Skip non-teal files and files with --check:false marker
# For compiled .lua files from .tl sources, check the original .tl file
$(o)/%.teal.ok: $(o)/% $$(cosmic_bin)
	@mkdir -p $(@D)
	@if echo "$<" | grep -qE '\.(tl|lua)$$'; then \
		check_file="$<"; \
		src_file="$${check_file#$(o)/}"; \
		if echo "$$src_file" | grep -qE '\.lua$$'; then \
			src_tl="$${src_file%.lua}.tl"; \
			if [ -f "$$src_tl" ]; then check_file="$$src_tl"; fi; \
		elif [ -f "$$src_file" ]; then \
			check_file="$$src_file"; \
		fi; \
		if head -10 "$$check_file" | grep -q -- '--check:false'; then \
			echo "ignore: check disabled" > $@; \
		else \
			if TL_PATH='$(TL_PATH)' $(cosmic_bin) --check "$$check_file" >/dev/null 2>$@.err; then \
				echo "pass:" > $@; \
			else \
				n=$$(grep -c ': error:' $@.err 2>/dev/null || echo 0); \
				echo "fail: $$n issues" > $@; \
				echo "" >> $@; echo "## stdout" >> $@; echo "" >> $@; \
				echo "## stderr" >> $@; echo "" >> $@; \
				grep ': error:' $@.err >> $@ 2>/dev/null || true; \
			fi; \
			rm -f $@.err; \
		fi; \
	else \
		echo "ignore: unsupported file type" > $@; \
	fi

## Run bun syntax checker on .gs/.js files
bun: $(o)/bun-summary.txt

$(o)/bun-summary.txt: $(all_bunned) | $(build_reporter)
	@$(reporter) --dir $(o) $^ | tee $@

.PHONY: clean
## Remove all build artifacts
clean:
	@rm -rf $(o)

.PHONY: bootstrap
## Bootstrap build environment
bootstrap: $(bootstrap_files)

all_checks := $(all_astgreps) $(all_teals) $(all_bunned)

## Run all linters (astgrep, teal, bun)
check: $(o)/check-summary.txt

$(o)/check-summary.txt: $(all_checks) | $(build_reporter)
	@$(reporter) --dir $(o) $^ | tee $@

## Verify all test files are declared in cook.mk
.PHONY: check-test-coverage
check-test-coverage: $(test_check_coverage) | $(bootstrap_files)
	@$(coverage_checker) $(all_declared_tests)

## Check for dependency updates
update: $(o)/update-summary.txt

$(o)/update-summary.txt: $(all_updated) | $(build_reporter)
	@$(reporter) --dir $(o) $^ | tee $@

$(o)/%.update.ok: % $(build_check_update) $(checker_files) | $(bootstrap_files)
	@mkdir -p $(@D)
	@$(update_runner) $< > $@

## Apply dependency updates (use with only=<module>)
.PHONY: bump
bump: $(all_updated)
	@for ok in $^; do \
	  ver=$${ok#$(o)/}; ver=$${ver%.update.ok}; \
	  $(update_runner) --apply "$$ok" "$$ver"; \
	done

.PHONY: build
## Build home, cosmic, and box binaries
build: home cosmic box

.PHONY: release
## Create release artifacts (CI only)
release:
	@mkdir -p release
	@cp artifacts/home-darwin-arm64/home release/home-darwin-arm64
	@cp artifacts/home-linux-arm64/home release/home-linux-arm64
	@cp artifacts/home-linux-x86_64/home release/home-linux-x86_64
	@cp artifacts/cosmopolitan/cosmic release/cosmic-lua
	@cp artifacts/cosmopolitan/box release/box
	@chmod +x release/*
	@tag="$$(date -u +%Y-%m-%d)-$${GITHUB_SHA::7}"; \
	(cd release && sha256sum home-* cosmic-lua box > SHA256SUMS && cat SHA256SUMS); \
	gh release create "$$tag" \
		$${PRERELEASE_FLAG} \
		--title "$$tag" \
		release/home-* release/cosmic-lua release/box release/SHA256SUMS

ci_stages := astgrep teal test build

.PHONY: ci
## Run full CI pipeline (astgrep, teal, test, build)
ci:
	@rm -f $(o)/failed
	@$(foreach s,$(ci_stages),\
		echo "::group::$(s)"; \
		$(MAKE) --keep-going $(s) || echo $(s) >> $(o)/failed; \
		echo "::endgroup::";)
	@if [ -f $(o)/failed ]; then echo "failed:"; cat $(o)/failed; exit 1; fi

debug-modules:
	@echo $(modules)

