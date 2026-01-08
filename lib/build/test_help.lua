#!/usr/bin/env run-test.lua
-- Test the Makefile help system

local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("cosmic.spawn").spawn

local tmpdir = TEST_TMPDIR
local test_bin = os.getenv("TEST_BIN")

-- helper: create temporary test makefile
local function create_test_makefile(content, name)
  name = name or "test.mk"
  local testfile = path.join(tmpdir, name)
  local f = assert(io.open(testfile, "w"))
  f:write(content)
  f:close()
  return testfile
end

-- helper: run help command
local function run_help(makefile, command)
  command = command or "help"
  local cosmic = path.join(test_bin, "cosmic")
  local help_script = path.join(test_bin, "make-help.lua")

  local handle = spawn({ cosmic, help_script, command, makefile })
  local stderr = handle.stderr and handle.stderr:read() or ""
  local ok, stdout = handle:read()
  return stdout or "", stderr
end

local function test_parse_multiline_comment()
  local makefile = create_test_makefile([[
## Run all tests
.PHONY: test
test:
	@echo "testing"

## Remove build artifacts
.PHONY: clean
clean:
	@rm -rf o
]], "multiline.mk")

  local stdout, stderr = run_help(makefile, "list")
  assert(stdout:find("test"), "expected test target")
  assert(stdout:find("clean"), "expected clean target")
end
test_parse_multiline_comment()

local function test_parse_multiline_continued()
  local makefile = create_test_makefile([[
## Run all tests
## with coverage enabled
.PHONY: test
test:
	@echo "testing"
]], "multiline-continued.mk")

  local stdout, stderr = run_help(makefile, "list")
  assert(stdout:find("test"), "expected test target")
end
test_parse_multiline_continued()

local function test_undocumented_targets_ignored()
  local makefile = create_test_makefile([[
## This target is documented
.PHONY: documented
documented:
	@echo "documented"

.PHONY: undocumented
undocumented:
	@echo "undocumented"
]], "undocumented.mk")

  local stdout, stderr = run_help(makefile, "list")
  assert(stdout:find("documented"), "expected documented target")
  assert(not stdout:find("undocumented"), "should not include undocumented target")
end
test_undocumented_targets_ignored()
