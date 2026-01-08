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

  local targets = help.parse_makefile(content)
  assert(#targets == 2, "expected 2 targets")
  assert(targets[1].name == "test", "expected test target")
  assert(targets[1].description == "Run all tests", "expected test description")
  assert(targets[2].name == "clean", "expected clean target")
  assert(targets[2].description == "Remove build artifacts", "expected clean description")
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

  local targets = help.parse_makefile(content)
  assert(#targets == 1, "expected 1 target")
  assert(targets[1].name == "test", "expected test target")
  assert(targets[1].description == "Run all tests with coverage enabled", "expected combined description")
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

  local targets = help.parse_makefile(content)
  assert(#targets == 1, "expected 1 target")
  assert(targets[1].name == "documented", "expected documented target")
end
test_undocumented_targets_ignored()

local function test_format_help()
  local targets = {
    { name = "test", description = "Run all tests" },
    { name = "clean", description = "Remove build artifacts" },
  }

  local output = help.format_help(targets)
  assert(output:find("Usage:"), "expected usage line")
  assert(output:find("Targets:"), "expected targets header")
  assert(output:find("test"), "expected test target")
  assert(output:find("clean"), "expected clean target")
  assert(output:find("only=PATTERN"), "expected options section")
end
test_format_help()
