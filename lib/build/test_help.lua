#!/usr/bin/env run-test.lua
-- Test the Makefile help system

local help = require("build.make-help")

local function test_parse_multiline_comment()
  local content = [[
## Run all tests
.PHONY: test
test:
	@echo "testing"

## Remove build artifacts
.PHONY: clean
clean:
	@rm -rf o
]]

  local items = help.parse_makefile(content)
  assert(#items == 2, "expected 2 items")
  assert(items[1].type == "target", "expected target type")
  assert(items[1].name == "test", "expected test target")
  assert(items[1].description == "Run all tests", "expected test description")
  assert(items[2].type == "target", "expected target type")
  assert(items[2].name == "clean", "expected clean target")
  assert(items[2].description == "Remove build artifacts", "expected clean description")
end
test_parse_multiline_comment()

local function test_parse_multiline_continued()
  local content = [[
## Run all tests
## with coverage enabled
.PHONY: test
test:
	@echo "testing"
]]

  local items = help.parse_makefile(content)
  assert(#items == 1, "expected 1 item")
  assert(items[1].type == "target", "expected target type")
  assert(items[1].name == "test", "expected test target")
  assert(items[1].description == "Run all tests with coverage enabled", "expected combined description")
end
test_parse_multiline_continued()

local function test_undocumented_targets_ignored()
  local content = [[
## This target is documented
.PHONY: documented
documented:
	@echo "documented"

.PHONY: undocumented
undocumented:
	@echo "undocumented"
]]

  local items = help.parse_makefile(content)
  assert(#items == 1, "expected 1 item")
  assert(items[1].type == "target", "expected target type")
  assert(items[1].name == "documented", "expected documented target")
end
test_undocumented_targets_ignored()

local function test_parse_variables()
  local content = [[
## Filter targets by pattern
filter-only = $(if $(only),$(foreach f,$1,$(if $(findstring $(only),$(f)),$(f))),$1)

## Set output directory
o := output
]]

  local items = help.parse_makefile(content)
  assert(#items == 2, "expected 2 items")
  assert(items[1].type == "option", "expected option type")
  assert(items[1].name == "filter-only", "expected filter-only option")
  assert(items[1].description == "Filter targets by pattern", "expected filter description")
  assert(items[2].type == "option", "expected option type")
  assert(items[2].name == "o", "expected o option")
end
test_parse_variables()

local function test_format_help()
  local items = {
    { type = "target", name = "test", description = "Run all tests" },
    { type = "target", name = "clean", description = "Remove build artifacts" },
    { type = "option", name = "only", description = "Filter by pattern" },
  }

  local output = help.format_help(items)
  assert(output:find("Usage:"), "expected usage line")
  assert(output:find("Targets:"), "expected targets header")
  assert(output:find("test"), "expected test target")
  assert(output:find("clean"), "expected clean target")
  assert(output:find("Options:"), "expected options header")
  assert(output:find("only"), "expected only option")
end
test_format_help()

local function test_makefile_snapshot()
  local cosmo = require("cosmo")

  -- Read actual Makefile
  local content = assert(cosmo.Slurp("Makefile"), "failed to read Makefile")

  local items = help.parse_makefile(content)
  local output = help.format_help(items)

  -- Expected snapshot of help output
  local expected = [[Usage: make [target] [options]

Targets:
  help                Show this help message
  fetched             Fetch all dependencies only
  staged              Fetch and extract all dependencies
  test                Run all tests (incremental)
  files               Build all module files
  astgrep             Run ast-grep linter on all files
  luacheck            Run luacheck linter on all files
  teal                Run teal type checker on all files
  clean               Remove all build artifacts
  check               Run all linters (astgrep, luacheck, teal)
  update              Check for dependency updates
  build               Build home and cosmic binaries
  release             Create release artifacts (CI only)
  ci                  Run full CI pipeline (luacheck, astgrep, teal, test, build)

Options:
  filter-only         Filter targets by pattern (make test only='skill')]]

  assert(output == expected, "help output does not match snapshot\nExpected:\n" .. expected .. "\n\nGot:\n" .. output)
end
test_makefile_snapshot()
