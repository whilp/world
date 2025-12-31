export LUA_PATH := lib/?.lua;lib/?/init.lua;lib/home/?.lua;;

include 3p/cook.mk
include 3p/luaunit/cook.mk
include 3p/luacheck/cook.mk
include 3p/make/cook.mk
include 3p/lua/cook.mk
include lib/home/cook.mk
include lib/test.mk

UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)
ifeq ($(UNAME_S),Darwin)
  PLATFORM := darwin-arm64
else ifeq ($(UNAME_M),aarch64)
  PLATFORM := linux-arm64
else
  PLATFORM := linux-x86_64
endif

ast_grep_dir := $(o)/$(PLATFORM)/3p/ast-grep
ast_grep_bin := $(ast_grep_dir)/bin/sg
ast_grep_extracted := $(ast_grep_dir)/.extracted

.DEFAULT_GOAL := build

help:
	@echo "Usage: make [target]"
	@echo ""
	@echo "Getting started:"
	@echo "  deps               Build dependencies (run this first)"
	@echo ""
	@echo "Build:"
	@echo "  build              Build lua binary [default]"
	@echo "  home               Build universal home binary"
	@echo "  platform-assets    Build platform-specific binaries"
	@echo ""
	@echo "Development:"
	@echo "  check              Run linters (ast-grep, luacheck)"
	@echo "  test               Run all tests (incremental)"
	@echo ""
	@echo "Maintenance:"
	@echo "  latest             Fetch latest versions (claude, cosmos, nvim)"
	@echo "  clean              Remove build artifacts and test stamps"
	@echo ""
	@echo "Run individual tests via stamp files:"
	@echo "  make o/lib/claude/test.lua.ok"

build: lua ## Build lua binary [default]

# bootstrap downloads a pre-built lua if results/bin/lua doesn't exist
# needed for fresh builds since cosmos rules require lua to run fetch.lua
bootstrap: private .PLEDGE = stdio rpath wpath cpath inet dns
bootstrap: private .INTERNET = 1
bootstrap:
	@if [ ! -f results/bin/lua ]; then \
		echo "Bootstrapping lua..."; \
		mkdir -p results/bin; \
		curl -fsSL -o results/bin/lua https://github.com/whilp/cosmopolitan/releases/latest/download/lua; \
		chmod +x results/bin/lua; \
	fi

deps: bootstrap lua ## Build dependencies (run this first)

latest: claude-latest cosmos-latest nvim-latest ## Fetch latest versions

clean: ## Remove build artifacts
clean: private .PLEDGE = stdio rpath wpath cpath
clean:
	rm -rf o results

$(foreach p,$(PLATFORMS),$(eval $(call platform_binaries_zip_rule,$(p))))

results:
	mkdir -p $@

check: ## Run linters (ast-grep, luacheck)
check: private .UNVEIL = r:$(CURDIR) rx:$(ast_grep_dir) rx:results/bin rw:/dev/null
check: private .PLEDGE = stdio rpath proc exec
check: private .CPU = 120
check: $(ast_grep_extracted) lua
	@echo "Validating Makefiles..."
	@bash -o pipefail -c '$(MAKE) -n --warn-undefined-variables build test clean 2>&1 | (! grep warning:)'
	$(ast_grep_bin) scan --color always
	@echo ""
	@echo "Running luacheck..."
	$(lua_bin) /zip/.lua/bin/luacheck \
		.config \
		lib \
		.github \
		--exclude-files '.claude/skills/lua/templates/*.lua' \
		--exclude-files '.config/nvim/**/*.lua' \
		--exclude-files '.config/hammerspoon/**/*.lua'
	@echo ""
	@$(MAKE) --no-print-directory check-test-coverage

.PHONY: help build bootstrap deps latest clean check
