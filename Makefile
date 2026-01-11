.SECONDEXPANSION:
.SECONDARY:
SHELL := /bin/bash
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

uname_s := $(shell uname -s)
uname_m := $(shell uname -m)
os := $(if $(filter Darwin,$(uname_s)),darwin,linux)
arch := $(subst aarch64,arm64,$(uname_m))
platforms := darwin-arm64 linux-arm64 linux-x86_64
platform := $(os)-$(arch)

include bootstrap.mk
include lib/cook.mk
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
include 3p/teal-types/cook.mk

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

# compile .tl files to .lua (extension changes)
$(o)/%.lua: %.tl $(types_files) $(tl_files) $(bootstrap_files) | $(tl_staged)
	@mkdir -p $(@D)
	@$(tl_gen) $< -o $@

# bin scripts: o/bin/X.lua from lib/*/X.lua and 3p/*/X.lua
vpath %.lua lib/build lib/test 3p/ast-grep 3p/luacheck 3p/tl
vpath %.tl 3p/ast-grep 3p/luacheck 3p/tl
$(o)/bin/%.lua: %.lua
	@mkdir -p $(@D)
	@$(cp) $< $@

# bin scripts from teal: o/bin/X.lua from 3p/*/X.tl (vpath finds X.tl)
$(o)/bin/%.lua: %.tl $(types_files) $(tl_files) $(bootstrap_files) | $(tl_staged)
	@mkdir -p $(@D)
	@$(tl_gen) $< -o $@

# files are produced in o/
all_files += $(call filter-only,$(foreach x,$(modules),$($(x)_files)))

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

all_tests := $(call filter-only,$(foreach x,$(modules),$($(x)_tests)))
all_tested := $(patsubst %,o/%.test.ok,$(all_tests))
all_snaps := $(call filter-only,$(foreach x,$(modules),$($(x)_snaps)))
all_snapped := $(patsubst %,$(o)/%.test.ok,$(all_snaps))

## Run all tests (incremental)
test: $(o)/test-summary.txt

$(o)/test-summary.txt: $(all_tested) $(all_snapped) | $(build_reporter)
	@$(reporter) --dir $(o) $^ | tee $@

export TEST_O := $(o)
export TEST_PLATFORM := $(platform)
export TEST_BIN := $(o)/bin
export LUA_PATH := $(CURDIR)/o/teal/lib/?.lua;$(CURDIR)/o/teal/lib/?/init.lua;$(CURDIR)/o/lib/?.lua;$(CURDIR)/o/lib/?/init.lua;$(CURDIR)/lib/?.lua;$(CURDIR)/lib/?/init.lua;;
export NO_COLOR := 1

$(o)/%.test.ok: .PLEDGE = stdio rpath wpath cpath proc exec
$(o)/%.test.ok: .UNVEIL = rx:$(o)/bootstrap r:lib r:3p rwc:$(o) rwc:/tmp rx:/usr rx:/proc r:/etc r:/dev/null
$(o)/%.test.ok: % $(test_files) $(checker_files) | $(bootstrap_files)
	@mkdir -p $(@D)
	@[ -x $< ] || chmod a+x $<
	@TEST_DIR=$(TEST_DIR) $(test_runner) $< > $@

# Snapshot test pattern: compare expected vs actual
$(o)/%.snap.test.ok: .EXTRA_PREREQS = $(build_snap)
$(o)/%.snap.test.ok: %.snap $(o)/%.snap | $(bootstrap_cosmic)
	@mkdir -p $(@D)
	@$(bootstrap_cosmic) $(build_snap) $< $(word 2,$^) > $@

# expand test deps: M's tests depend on own _files/_dir plus deps' _dir
$(foreach m,$(filter-out bootstrap,$(modules)),\
  $(eval $(patsubst %,$(o)/%.test.ok,$($(m)_tests)): $($(m)_files))\
  $(eval $(patsubst %,$(o)/%.test.ok,$($(m)_tests)): TEST_DEPS += $($(m)_files))\
  $(if $($(m)_dir),\
    $(eval $(patsubst %,$(o)/%.test.ok,$($(m)_tests)): $($(m)_dir))\
    $(eval $(patsubst %,$(o)/%.test.ok,$($(m)_tests)): TEST_DEPS += $($(m)_dir))\
    $(eval $(patsubst %,$(o)/%.test.ok,$($(m)_tests)): TEST_DIR := $($(m)_dir)))\
  $(foreach d,$(filter-out $(m),$(default_deps) $($(m)_deps)),\
    $(if $($(d)_dir),\
      $(eval $(patsubst %,$(o)/%.test.ok,$($(m)_tests)): $($(d)_dir))\
      $(eval $(patsubst %,$(o)/%.test.ok,$($(m)_tests)): TEST_DEPS += $($(d)_dir)))))

all_built_files := $(call filter-only,$(foreach x,$(modules),$($(x)_files)))
all_source_files := $(call filter-only,$(foreach x,$(modules),$($(x)_tests)))
all_source_files += $(call filter-only,$(filter-out ,$(foreach x,$(modules),$($(x)_version))))
all_source_files += $(call filter-only,$(foreach x,$(modules),$($(x)_srcs)))
all_checkable_files := $(addprefix $(o)/,$(all_source_files))

.PHONY: files
## Build all module files
files: $(all_built_files)

all_astgreps := $(patsubst %,%.ast-grep.ok,$(all_checkable_files))

## Run ast-grep linter on all files
astgrep: $(o)/astgrep-summary.txt

$(o)/astgrep-summary.txt: $(all_astgreps) | $(build_reporter)
	@$(reporter) --dir $(o) $^ | tee $@

$(o)/%.ast-grep.ok: $(o)/% $(ast-grep_files) $(checker_files) $(tl_staged) | $(bootstrap_files) $(ast-grep_staged)
	@mkdir -p $(@D)
	@ASTGREP_BIN=$(ast-grep_staged) $(astgrep_runner) $< > $@

all_luachecks := $(patsubst %,%.luacheck.ok,$(all_checkable_files))

## Run luacheck linter on all files
luacheck: $(o)/luacheck-summary.txt

$(o)/luacheck-summary.txt: $(all_luachecks) | $(build_reporter)
	@$(reporter) --dir $(o) $^ | tee $@

$(o)/%.luacheck.ok: $(o)/% $(luacheck_files) $(checker_files) $(tl_staged) | $(bootstrap_files) $(luacheck_staged)
	@mkdir -p $(@D)
	@LUACHECK_BIN=$(luacheck_staged) $(luacheck_runner) $< > $@

all_teals := $(patsubst %,%.teal.ok,$(all_checkable_files))

## Run teal type checker on all files
teal: $(o)/teal-summary.txt

$(o)/teal-summary.txt: $(all_teals) | $(build_reporter)
	@$(reporter) --dir $(o) $^ | tee $@

$(o)/%.teal.ok: $(o)/% $(tl_files) $(checker_files) $(tl_staged) $$(teal-types_staged) | $(bootstrap_files)
	@mkdir -p $(@D)
	@TL_BIN=$(tl_staged) TL_INCLUDE_DIR="lib/types:$(teal-types_dir)/types" $(teal_runner) $< > $@

.PHONY: clean
## Remove all build artifacts
clean:
	@rm -rf $(o)

.PHONY: bootstrap
## Bootstrap build environment
bootstrap: $(bootstrap_files)

all_checks := $(all_astgreps) $(all_luachecks) $(all_teals)

## Run all linters (astgrep, luacheck, teal)
check: $(o)/check-summary.txt

$(o)/check-summary.txt: $(all_checks) | $(build_reporter)
	@$(reporter) --dir $(o) $^ | tee $@

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
## Build home and cosmic binaries
build: home cosmic

.PHONY: release
## Create release artifacts (CI only)
release: $(o)/lib/home/gen-platforms.lua
	@mkdir -p release
	@cp artifacts/home-darwin-arm64/home release/home-darwin-arm64
	@cp artifacts/home-linux-arm64/home release/home-linux-arm64
	@cp artifacts/home-linux-x86_64/home release/home-linux-x86_64
	@cp artifacts/home-linux-x86_64/home release/home
	@cp artifacts/cosmic/cosmic release/cosmic-lua
	@chmod +x release/*
	@chmod +x artifacts/cosmos-zip/zip
	@tag="home-$$(date -u +%Y-%m-%d)-$${GITHUB_SHA::7}"; \
	base_url="https://github.com/$${GITHUB_REPOSITORY}/releases/download/$$tag"; \
	LUA_PATH="lib/home/?.lua;;" ./release/cosmic-lua $(o)/lib/home/gen-platforms.lua \
		release/platforms "$$base_url" "$$tag" \
		release/home-darwin-arm64 release/home-linux-arm64 release/home-linux-x86_64; \
	(cd release/platforms && ../../artifacts/cosmos-zip/zip -j ../home platforms.lua); \
	(cd release/platforms && ../../artifacts/cosmos-zip/zip -r ../home manifests); \
	(cd release && sha256sum home home-* cosmic-lua > SHA256SUMS && cat SHA256SUMS); \
	gh release create "$$tag" \
		$${PRERELEASE_FLAG} \
		--title "home $$tag" \
		--notes "## Home binaries\nPlatform-specific dotfiles and bundled tools.\n\n### Quick setup\n\`\`\`bash\ncurl -fsSL https://github.com/$${GITHUB_REPOSITORY}/releases/latest/download/home | sh\n\`\`\`" \
		release/home release/home-* release/cosmic-lua release/SHA256SUMS

ci_stages := luacheck astgrep teal test build

.PHONY: ci
## Run full CI pipeline (luacheck, astgrep, teal, test, build)
ci:
	@rm -f $(o)/failed
	@$(foreach s,$(ci_stages),\
		echo "::group::$(s)"; \
		$(MAKE) --keep-going $(s) || echo $(s) >> $(o)/failed; \
		echo "::endgroup::";)
	@if [ -f $(o)/failed ]; then echo "failed:"; cat $(o)/failed; exit 1; fi

debug-modules:
	@echo $(modules)

