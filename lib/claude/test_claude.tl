local unix = require("cosmo.unix")
local path = require("cosmo.path")
local claude = require("claude.main")

global TEST_TMPDIR: string

local function test_find_claude_binary_finds_existing()
  local tmpfile = path.join(TEST_TMPDIR, "testfile")
  local f = io.open(tmpfile, "w")
  if f then
    f:write("test")
    f:close()
  end

  local paths = {"/nonexistent", tmpfile, "/also/nonexistent"}
  local result = claude.find_claude_binary(paths)

  assert(result == tmpfile, "should find existing file")
  unix.unlink(tmpfile)
end
test_find_claude_binary_finds_existing()

local function test_find_claude_binary_returns_nil_when_none_exist()
  local paths = {"/nonexistent1", "/nonexistent2"}
  local result = claude.find_claude_binary(paths)

  assert(result == nil, "should return nil when no files exist")
end
test_find_claude_binary_returns_nil_when_none_exist()

local function test_find_claude_binary_handles_nil_in_paths()
  local tmpfile = path.join(TEST_TMPDIR, "testfile2")
  local f = io.open(tmpfile, "w")
  if f then
    f:write("test")
    f:close()
  end

  local paths: {string} = {nil as string, tmpfile, "/also/nonexistent"}
  local result = claude.find_claude_binary(paths)

  assert(result == tmpfile, "should find existing file even when nil is first element")
  unix.unlink(tmpfile)
end
test_find_claude_binary_handles_nil_in_paths()

local function test_build_argv_basic()
  local argv = claude.build_argv({}, nil, {})

  assert(#argv == 2, "should have base arguments, got " .. #argv)
  assert(argv[1] == "--dangerously-skip-permissions")
  assert(argv[2] == "--strict-mcp-config")
end
test_build_argv_basic()

local function test_build_argv_with_append_prompt()
  local argv = claude.build_argv({"prompt1", "prompt2"}, nil, {})
  local joined = table.concat(argv, " ")

  assert(joined:find("append-system-prompt", 1, true), "should contain append-system-prompt")
  assert(#argv == 4, "should have base args plus append prompt args, got " .. #argv)
end
test_build_argv_with_append_prompt()

local function test_build_argv_with_user_args()
  local argv = claude.build_argv({}, nil, {"--help", "test"})

  assert(#argv >= 4, "should include user args")
  assert(argv[#argv - 1] == "--help")
  assert(argv[#argv] == "test")
end
test_build_argv_with_user_args()

local function test_build_argv_with_mcp_config()
  local tmpfile = path.join(TEST_TMPDIR, "mcp.json")
  local f = io.open(tmpfile, "w")
  if f then
    f:write("{}")
    f:close()
  end

  local argv = claude.build_argv({}, tmpfile, {})
  local joined = table.concat(argv, " ")

  assert(joined:find("mcp-config", 1, true), "should contain mcp-config")
  assert(joined:find(tmpfile, 1, true), "should contain mcp config path")

  unix.unlink(tmpfile)
end
test_build_argv_with_mcp_config()

local function test_build_argv_ignores_nonexistent_mcp()
  local argv = claude.build_argv({}, "/nonexistent/mcp.json", {})

  assert(#argv == 2, "should not add mcp config if file doesn't exist, got " .. #argv)
end
test_build_argv_ignores_nonexistent_mcp()

local function test_scan_for_claude_deploy()
  local result = claude.scan_for_claude_deploy()
  assert(result == nil or type(result) == "string", "should return nil or string")
end
test_scan_for_claude_deploy()

local function test_scan_for_atomic_install()
  local result = claude.scan_for_atomic_install()
  assert(result == nil or type(result) == "string", "should return nil or string")
end
test_scan_for_atomic_install()
