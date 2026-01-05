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

local function write_result(status, message, stdout, stderr)
  local lines = {}
  if message and message ~= "" then
    table.insert(lines, status .. ": " .. message)
  else
    table.insert(lines, status)
  end
  table.insert(lines, "")
  table.insert(lines, "## stdout")
  table.insert(lines, "")
  table.insert(lines, stdout)
  table.insert(lines, "## stderr")
  table.insert(lines, "")
  table.insert(lines, stderr)
  local output = table.concat(lines, "\n")
  if status == "fail" then
    io.stderr:write(output)
    return 1
  else
    io.write(output)
    return 0
  end
end

local function main(test)
  if not test then
    return 1, "usage: run-test.lua <test>"
  end

  local ok, err, stdout, stderr = run_test(test)

  local status, message
  if ok then
    status = "pass"
  else
    local err_str = tostring(err)
    -- check for SKIP or IGNORE in error message
    local skip_reason = err_str:match("SKIP%s+(.+)")
    local ignore_reason = err_str:match("IGNORE%s+(.+)")
    if skip_reason then
      status = "skip"
      message = skip_reason
    elseif ignore_reason then
      status = "ignore"
      message = ignore_reason
    else
      status = "fail"
      -- strip path prefix to show just filename:line: message
      message = err_str:gsub("^.-/([^/]+:%d+:)", "%1")
    end
  end

  return write_result(status, message, stdout, stderr)
end

if cosmo.is_main() then
  -- TODO: use varargs once bootstrap cosmic is updated
  local code, err = main(arg[1], arg[2])
  if err then
    io.stderr:write(err .. "\n")
  end
  os.exit(code)
end
