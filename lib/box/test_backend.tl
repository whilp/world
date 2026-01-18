#!/usr/bin/env run-test.lua
-- conformance tests for box executable backends

local spawn = require("cosmic.spawn")

global TEST_TMPDIR: string

local backend = "lib/box/example-backend"

local function run(args: {string}): number, string
  local cmd = {backend}
  for _, arg in ipairs(args) do
    cmd[#cmd + 1] = arg
  end
  local handle = spawn(cmd, {stderr = -1})
  if not handle then
    return -1, "failed to spawn"
  end
  local exit_code = handle:wait()
  local err = ""
  if handle.stderr then
    local ok, content = handle.stderr:read()
    if ok and content then
      err = content as string
    end
  end
  return exit_code, err
end

local function test_invalid_command()
  local code = run({"invalid-command"})
  assert(code ~= 0, "invalid command should return non-zero exit code")
end
test_invalid_command()

local function test_list()
  local code = run({"list"})
  assert(code == 0, "list should succeed")
end
test_list()

local function test_new_requires_name()
  local code = run({"new"})
  assert(code ~= 0, "new without name should fail")
end
test_new_requires_name()

local function test_new()
  local code = run({"new", "test-box"})
  assert(code == 0, "new with name should succeed")
end
test_new()

local function test_ssh_requires_name()
  local code = run({"ssh"})
  assert(code ~= 0, "ssh without name should fail")
end
test_ssh_requires_name()

local function test_ssh()
  local code = run({"ssh", "test-box"})
  assert(code == 0, "ssh with name should succeed")
end
test_ssh()

local function test_exec_requires_args()
  local code = run({"exec"})
  assert(code ~= 0, "exec without args should fail")

  code = run({"exec", "test-box"})
  assert(code ~= 0, "exec without command should fail")
end
test_exec_requires_args()

local function test_exec()
  local code = run({"exec", "test-box", "echo hello"})
  assert(code == 0, "exec with args should succeed")
end
test_exec()

local function test_upload_requires_args()
  local code = run({"upload"})
  assert(code ~= 0, "upload without args should fail")

  code = run({"upload", "test-box"})
  assert(code ~= 0, "upload without src/dst should fail")

  code = run({"upload", "test-box", "/tmp/src"})
  assert(code ~= 0, "upload without dst should fail")
end
test_upload_requires_args()

local function test_upload()
  local code = run({"upload", "test-box", "/tmp/src", "/tmp/dst"})
  assert(code == 0, "upload with args should succeed")
end
test_upload()

local function test_download_requires_args()
  local code = run({"download"})
  assert(code ~= 0, "download without args should fail")

  code = run({"download", "test-box"})
  assert(code ~= 0, "download without src/dst should fail")

  code = run({"download", "test-box", "/tmp/src"})
  assert(code ~= 0, "download without dst should fail")
end
test_download_requires_args()

local function test_download()
  local code = run({"download", "test-box", "/tmp/src", "/tmp/dst"})
  assert(code == 0, "download with args should succeed")
end
test_download()

local function test_destroy_requires_name()
  local code = run({"destroy"})
  assert(code ~= 0, "destroy without name should fail")
end
test_destroy_requires_name()

local function test_destroy()
  local code = run({"destroy", "test-box"})
  assert(code == 0, "destroy with name should succeed")
end
test_destroy()
