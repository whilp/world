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

ast_grep_dir := $(3p)/ast-grep
ast_grep_bin := $(ast_grep_dir)/$(PLATFORM)/sg
ast_grep_extracted := $(ast_grep_dir)/$(PLATFORM)/.extracted

.DEFAULT_GOAL := build

help: ## Show available targets
	@echo "Usage: make [target]"
	@echo ""
	@grep -hE '^[a-zA-Z_-]+:.*## ' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*## "}; {printf "  %-18s %s\n", $$1, $$2}' | sort
	@echo ""
	@echo "Individual test targets:"
	@for t in $(TEST_TARGETS); do printf "  %s\n" "$$t"; done

build: lua ## Build lua binary [default]

clean: ## Remove o/ and results/
clean: private .PLEDGE = stdio rpath wpath cpath
clean:
	rm -rf o results

$(foreach p,$(PLATFORMS),$(eval $(call platform_binaries_zip_rule,$(p))))

results:
	mkdir -p $@

check: ## Run linters (ast-grep, luacheck)
check: private .UNVEIL = r:$(CURDIR) rx:$(3p)/ast-grep rx:results/bin rw:/dev/null
check: private .PLEDGE = stdio rpath proc exec
check: private .CPU = 120
check: $(ast_grep_extracted) lua
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

.PHONY: help build clean check
