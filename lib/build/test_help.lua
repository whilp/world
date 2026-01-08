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

-- helper: run help-check command
local function run_help_check(makefile)
  local cosmic = path.join(test_bin, "cosmic")
  local check_script = path.join(test_bin, "make-check-help.lua")

  local handle = spawn({ cosmic, check_script, makefile })
  local stderr = handle.stderr and handle.stderr:read() or ""
  local ok, stdout, exit_code = handle:read()
  return exit_code, stdout or "", stderr
end

local function test_parse_inline_comment()
  local makefile = create_test_makefile([[
.PHONY: test
test: ## Run all tests
	@echo "testing"

.PHONY: clean
clean: ## Remove build artifacts
	@rm -rf o
]], "inline.mk")

  local stdout, stderr = run_help(makefile, "list")
  assert(stdout:find("test"), "expected 'test' in output")
  assert(stdout:find("clean"), "expected 'clean' in output")
end
test_parse_inline_comment()

local function test_parse_section_headers()
  local makefile = create_test_makefile([[
## @ Build targets

.PHONY: build
build: ## Build the project
	@echo "building"

## @ Test targets

.PHONY: test
test: ## Run tests
	@echo "testing"
]], "sections.mk")

  local stdout, stderr = run_help(makefile, "help")
  assert(stdout:find("Build targets"), "expected Build targets section")
  assert(stdout:find("Test targets"), "expected Test targets section")
  assert(stdout:find("build"), "expected build target")
  assert(stdout:find("test"), "expected test target")
end
test_parse_section_headers()

local function test_parse_multiline_comment()
  local makefile = create_test_makefile([[
## Run all tests
## with coverage enabled
.PHONY: test
test:
	@echo "testing"
]], "multiline.mk")

  local stdout, stderr = run_help(makefile, "list")
  assert(stdout:find("test"), "expected test target")
end
test_parse_multiline_comment()

local function test_undocumented_targets_ignored()
  local makefile = create_test_makefile([[
.PHONY: documented
documented: ## This target is documented
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

local function test_validation_passes()
  local makefile = create_test_makefile([[
.PHONY: test
test: ## Run tests
	@echo "testing"
]], "valid.mk")

  local exit_code, stdout, stderr = run_help_check(makefile)
  assert(exit_code == 0, "expected validation to pass")
end
test_validation_passes()
