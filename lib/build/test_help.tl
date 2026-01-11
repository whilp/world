#!/usr/bin/env run-test.lua
-- Test the Makefile help system

local cosmo = require("cosmo")
local help = require("make-help")

local record HelpItem
  type: string
  name: string
  description: string
end

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

  local items = help.parse_makefile(content) as {HelpItem}
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

  local items = help.parse_makefile(content) as {HelpItem}
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

  local items = help.parse_makefile(content) as {HelpItem}
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

  local items = help.parse_makefile(content) as {HelpItem}
  assert(#items == 2, "expected 2 items")
  assert(items[1].type == "option", "expected option type")
  assert(items[1].name == "filter-only", "expected filter-only option")
  assert(items[1].description == "Filter targets by pattern", "expected filter description")
  assert(items[2].type == "option", "expected option type")
  assert(items[2].name == "o", "expected o option")
end
test_parse_variables()

local function test_format_help()
  local items: {HelpItem} = {
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

local function test_parse_phony_targets()
  local content = [[
.PHONY: test clean
test:
	@echo "testing"

.PHONY: build
build:
	@echo "building"
]]

  local targets = help.parse_phony_targets(content) as {string:boolean}
  assert(targets["test"], "expected test target")
  assert(targets["clean"], "expected clean target")
  assert(targets["build"], "expected build target")
  assert(not targets["foo"], "unexpected foo target")
end
test_parse_phony_targets()

local function test_all_phony_targets_documented()
  local content, err = cosmo.Slurp("Makefile")
  assert(content, err or "failed to read Makefile")

  local phony = help.parse_phony_targets(content) as {string:boolean}
  local items = help.parse_makefile(content) as {HelpItem}

  local documented: {string:boolean} = {}
  for _, item in ipairs(items) do
    if item.type == "target" then
      documented[item.name] = true
    end
  end

  local missing: {string} = {}
  for target in pairs(phony) do
    if not documented[target] then
      table.insert(missing, target)
    end
  end

  if #missing > 0 then
    table.sort(missing)
    error("phony targets missing documentation: " .. table.concat(missing, ", "))
  end
end
test_all_phony_targets_documented()
