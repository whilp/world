#!/usr/bin/env lua
-- teal ignore: type annotations needed

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")

local function run_test(test)
  TEST_TMPDIR = unix.mkdtemp("/tmp/test_XXXXXX")
  TEST_DIR = os.getenv("TEST_DIR")

  -- create temp files for capturing output
  local stdout_file = path.join(TEST_TMPDIR, "stdout")
  local stderr_file = path.join(TEST_TMPDIR, "stderr")

  -- save original fds (dup returns lowest available fd, so these go to 3+)
  local orig_stdout = unix.dup(1)
  local orig_stderr = unix.dup(2)

  -- open capture files
  local stdout_fd = unix.open(stdout_file, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("644", 8))
  local stderr_fd = unix.open(stderr_file, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("644", 8))

  -- redirect stdout: close fd 1, then dup stdout_fd (becomes 1)
  unix.close(1)
  unix.dup(stdout_fd)
  unix.close(stdout_fd)

  -- redirect stderr: close fd 2, then dup stderr_fd (becomes 2)
  unix.close(2)
  unix.dup(stderr_fd)
  unix.close(stderr_fd)

  -- run the test
  local ok, err = pcall(dofile, test)

  -- flush lua's io buffers before restoring fds
  io.stdout:flush()
  io.stderr:flush()

  -- restore stdout: close fd 1, dup orig_stdout (becomes 1)
  unix.close(1)
  unix.dup(orig_stdout)
  unix.close(orig_stdout)

  -- restore stderr: close fd 2, dup orig_stderr (becomes 2)
  unix.close(2)
  unix.dup(orig_stderr)
  unix.close(orig_stderr)

  -- read captured output
  local stdout = cosmo.Slurp(stdout_file) or ""
  local stderr = cosmo.Slurp(stderr_file) or ""

  unix.rmrf(TEST_TMPDIR)

  return ok, err, stdout, stderr
end

local function format_output(result, message, stdout, stderr)
  local lines = {}
  if message and message ~= "" then
    table.insert(lines, result .. ": " .. message)
  else
    table.insert(lines, result)
  end
  table.insert(lines, "")
  table.insert(lines, "## stdout")
  table.insert(lines, "")
  table.insert(lines, stdout)
  table.insert(lines, "## stderr")
  table.insert(lines, "")
  table.insert(lines, stderr)
  return table.concat(lines, "\n")
end

local function main(test)
  if not test then
    return 1, "usage: run-test.lua <test>"
  end

  local ok, err, stdout, stderr = run_test(test)

  local result, message
  if ok then
    result = "pass"
  else
    local err_str = tostring(err)
    -- check for SKIP in error message
    local skip_reason = err_str:match("SKIP%s+(.+)")
    if skip_reason then
      result = "skip"
      message = skip_reason
    else
      result = "fail"
      -- strip path prefix to show just filename:line: message
      message = err_str:gsub("^.-/([^/]+:%d+:)", "%1")
    end
  end

  io.write(format_output(result, message, stdout, stderr))
  return result == "fail" and 1 or 0
end

if cosmo.is_main() then
  local code, err = main(...)
  if err then
    io.stderr:write(err .. "\n")
  end
  os.exit(code)
end
