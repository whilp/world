#!/usr/bin/env run-test.lua

local path = require("cosmo.path")
local unix = require("cosmo.unix")
local cosmo = require("cosmo")

local tmpdir = os.getenv("TEST_TMPDIR") or "/tmp"

local function write_result_file(filepath: string, status: string, message: string, stdout: string, stderr: string)
  local content = status
  if message then
    content = content .. ": " .. message
  end
  content = content .. "\n\n## stdout\n"
  if stdout then
    content = content .. stdout
  end
  content = content .. "\n\n## stderr\n"
  if stderr then
    content = content .. stderr
  end
  local dir = path.dirname(filepath)
  if dir and dir ~= "" and dir ~= "." then
    unix.makedirs(dir)
  end
  cosmo.Barf(filepath, content)
end

local record SpawnHandle
  stderr: FILE
  read: function(self: SpawnHandle): boolean, string, integer
end

local function run_reporter(...: string): integer, string, string
  local test_bin = os.getenv("TEST_BIN") or ""
  local reporter = path.join(test_bin, "reporter.lua")
  local spawn = require("cosmic.spawn").spawn
  local args: {string} = {path.join(test_bin, "cosmic"), "--", reporter}
  local varargs: {string} = {...}
  for _, arg in ipairs(varargs) do
    table.insert(args, arg)
  end

  local lua_path = os.getenv("LUA_PATH") or ""
  local new_lua_path = "o/lib/?.lua;o/lib/?/init.lua;;" .. lua_path
  unix.setenv("LUA_PATH", new_lua_path, true)

  local handle = spawn(args) as SpawnHandle
  local stderr_content = handle.stderr and handle.stderr:read() or ""
  local ok, stdout, exit_code = handle:read()
  return exit_code, stdout or "", stderr_content
end

-- test single checker mode (test)
local function test_single_checker_mode()
  local test_dir = path.join(tmpdir, "single-checker")
  unix.makedirs(test_dir)

  write_result_file(path.join(test_dir, "test1.test.ok"), "pass", nil, "", "")
  write_result_file(path.join(test_dir, "test2.test.ok"), "fail", "assertion failed", "", "expected true")

  local code, stdout, stderr = run_reporter(
    "--dir", test_dir,
    path.join(test_dir, "test1.test.ok"),
    path.join(test_dir, "test2.test.ok")
  )

  assert(code == 1, "expected exit code 1 for failures, got: " .. tostring(code))
  assert(stdout:match("PASS"), "expected PASS in output")
  assert(stdout:match("FAIL"), "expected FAIL in output")
  assert(stdout:match("test: 2 checks: 1 passed, 1 failed"), "expected test summary")
  assert(stdout:match("FAILURES"), "expected failures section")
end
test_single_checker_mode()

-- test multi-checker mode (check)
local function test_multi_checker_mode()
  local test_dir = path.join(tmpdir, "multi-checker")
  unix.makedirs(test_dir)

  write_result_file(path.join(test_dir, "file1.ast-grep.ok"), "pass", nil, "", "")
  write_result_file(path.join(test_dir, "file1.luacheck.ok"), "pass", nil, "", "")
  write_result_file(path.join(test_dir, "file2.teal.ok"), "fail", "type error", "", "Expected string")

  local code, stdout, stderr = run_reporter(
    "--dir", test_dir,
    path.join(test_dir, "file1.ast-grep.ok"),
    path.join(test_dir, "file1.luacheck.ok"),
    path.join(test_dir, "file2.teal.ok")
  )

  assert(code == 1, "expected exit code 1 for failures")
  assert(stdout:match("ast%-grep: 1 checks"), "expected ast-grep summary")
  assert(stdout:match("luacheck: 1 checks"), "expected luacheck summary")
  assert(stdout:match("teal: 1 checks"), "expected teal summary")
  assert(stdout:match("total: 3 checks: 2 passed, 1 failed"), "expected total summary")
end
test_multi_checker_mode()

-- test update mode custom summary
local function test_update_mode_summary()
  local test_dir = path.join(tmpdir, "update-mode")
  unix.makedirs(test_dir)

  write_result_file(path.join(test_dir, "v1.update.ok"), "pass", "1.0.0", "", "")
  write_result_file(path.join(test_dir, "v2.update.ok"), "skip", "1.0.0 -> 2.0.0", "", "")

  local code, stdout, stderr = run_reporter(
    "--dir", test_dir,
    path.join(test_dir, "v1.update.ok"),
    path.join(test_dir, "v2.update.ok")
  )

  assert(code == 0, "expected exit code 0 for updates")
  assert(stdout:match("update: 2 checks: 1 passed, 0 failed, 1 skipped"), "expected update summary format")
  assert(stdout:match("SKIP.*1%.0%.0 %-> 2%.0%.0"), "expected skip with version info")
end
test_update_mode_summary()

-- test directory stripping
local function test_directory_stripping()
  local test_dir = path.join(tmpdir, "dir-strip")
  unix.makedirs(test_dir)

  write_result_file(path.join(test_dir, "subdir", "file.test.ok"), "pass", nil, "", "")

  local code, stdout, stderr = run_reporter(
    "--dir", test_dir,
    path.join(test_dir, "subdir", "file.test.ok")
  )

  assert(code == 0, "expected exit code 0")
  assert(stdout:match("subdir/file"), "expected directory to be stripped")
  assert(not stdout:match(test_dir), "expected tmpdir not in output")
end
test_directory_stripping()

-- test skip and ignore statuses
local function test_skip_and_ignore()
  local test_dir = path.join(tmpdir, "skip-ignore")
  unix.makedirs(test_dir)

  write_result_file(path.join(test_dir, "t1.test.ok"), "pass", nil, "", "")
  write_result_file(path.join(test_dir, "t2.test.ok"), "skip", "not implemented", "", "")
  write_result_file(path.join(test_dir, "t3.test.ok"), "ignore", "unsupported", "", "")

  local code, stdout, stderr = run_reporter(
    "--dir", test_dir,
    path.join(test_dir, "t1.test.ok"),
    path.join(test_dir, "t2.test.ok"),
    path.join(test_dir, "t3.test.ok")
  )

  assert(code == 0, "expected exit code 0")
  assert(stdout:match("test: 3 checks: 1 passed, 0 failed, 1 skipped, 1 ignored"), "expected correct counts")
  assert(not stdout:match("FAILURES"), "expected no failures section")
end
test_skip_and_ignore()

-- test missing --dir flag
local function test_missing_dir_flag()
  local code, stdout, stderr = run_reporter("file.test.ok")

  assert(code == 1, "expected exit code 1")
  assert(stderr:match("%-%-dir is required"), "expected error about --dir")
end
test_missing_dir_flag()

-- test no files specified
local function test_no_files()
  local code, stdout, stderr = run_reporter("--dir", tmpdir)

  assert(code == 1, "expected exit code 1")
  assert(stderr:match("usage:"), "expected usage error")
end
test_no_files()

-- test auto-detection of checker types
local function test_checker_auto_detection()
  local test_dir = path.join(tmpdir, "auto-detect")
  unix.makedirs(test_dir)

  write_result_file(path.join(test_dir, "file.custom-checker.ok"), "pass", nil, "", "")

  local code, stdout, stderr = run_reporter(
    "--dir", test_dir,
    path.join(test_dir, "file.custom-checker.ok")
  )

  assert(code == 0, "expected exit code 0")
  assert(stdout:match("custom%-checker: 1 checks"), "expected custom checker to be auto-detected")
end
test_checker_auto_detection()
