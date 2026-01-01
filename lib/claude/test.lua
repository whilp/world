local lu = require('luaunit')
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local claude = require("claude.main")

function test_find_claude_binary_finds_existing()
  local tmpfile = path.join(TEST_TMPDIR, "testfile")
  local f = io.open(tmpfile, "w")
  f:write("test")
  f:close()

  local paths = {"/nonexistent", tmpfile, "/also/nonexistent"}
  local result = claude.find_claude_binary(paths)

  lu.assertEquals(result, tmpfile, "should find existing file")
  unix.unlink(tmpfile)
end

function test_find_claude_binary_returns_nil_when_none_exist()
  local paths = {"/nonexistent1", "/nonexistent2"}
  local result = claude.find_claude_binary(paths)

  lu.assertNil(result, "should return nil when no files exist")
end

function test_find_claude_binary_handles_nil_in_paths()
  local tmpfile = path.join(TEST_TMPDIR, "testfile2")
  local f = io.open(tmpfile, "w")
  f:write("test")
  f:close()

  local paths = {nil, tmpfile, "/also/nonexistent"}
  local result = claude.find_claude_binary(paths)

  lu.assertEquals(result, tmpfile, "should find existing file even when nil is first element")
  unix.unlink(tmpfile)
end

function test_build_argv_basic()
  local argv = claude.build_argv({}, nil, {})

  lu.assertEquals(#argv, 2, "should have base arguments")
  lu.assertEquals(argv[1], "--dangerously-skip-permissions")
  lu.assertEquals(argv[2], "--strict-mcp-config")
end

function test_build_argv_with_append_prompt()
  local argv = claude.build_argv({"prompt1", "prompt2"}, nil, {})

  lu.assertStrContains(table.concat(argv, " "), "--append-system-prompt")
  lu.assertEquals(#argv, 4, "should have base args plus append prompt args")
end

function test_build_argv_with_user_args()
  local argv = claude.build_argv({}, nil, {"--help", "test"})

  lu.assertTrue(#argv >= 4, "should include user args")
  lu.assertEquals(argv[#argv - 1], "--help")
  lu.assertEquals(argv[#argv], "test")
end

function test_build_argv_with_mcp_config()
  local tmpfile = path.join(TEST_TMPDIR, "mcp.json")
  local f = io.open(tmpfile, "w")
  f:write("{}")
  f:close()

  local argv = claude.build_argv({}, tmpfile, {})

  lu.assertStrContains(table.concat(argv, " "), "--mcp-config")
  lu.assertStrContains(table.concat(argv, " "), tmpfile)

  unix.unlink(tmpfile)
end

function test_build_argv_ignores_nonexistent_mcp()
  local argv = claude.build_argv({}, "/nonexistent/mcp.json", {})

  lu.assertEquals(#argv, 2, "should not add mcp config if file doesn't exist")
end

function test_scan_for_claude_deploy()
  local result = claude.scan_for_claude_deploy()
  lu.assertTrue(result == nil or type(result) == "string", "should return nil or string")
end

function test_scan_for_atomic_install()
  local result = claude.scan_for_atomic_install()
  lu.assertTrue(result == nil or type(result) == "string", "should return nil or string")
end

