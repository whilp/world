#!/usr/bin/env cosmic
local fs = require("cosmic.fs")
local env = require("cosmic.env")
local spawn = require("cosmic.child")

local world = "o/world"
local main = require("main")

--- unit tests: parse_args

local function test_parse_args_defaults()
  local opts = main.parse_args({})
  assert(opts.output == "", "expected empty output by default")
  assert(opts.list == false, "expected list false by default")
  assert(opts.help == false, "expected help false by default")
end
test_parse_args_defaults()

local function test_parse_args_output()
  local opts = main.parse_args({"--output", "/tmp/test.tar"})
  assert(opts.output == "/tmp/test.tar", "expected output /tmp/test.tar, got " .. opts.output)
end
test_parse_args_output()

local function test_parse_args_output_short()
  local opts = main.parse_args({"-o", "/tmp/short.tar"})
  assert(opts.output == "/tmp/short.tar", "expected output /tmp/short.tar, got " .. opts.output)
end
test_parse_args_output_short()

local function test_parse_args_list()
  local opts = main.parse_args({"--list"})
  assert(opts.list == true, "expected list true")
end
test_parse_args_list()

local function test_parse_args_list_short()
  local opts = main.parse_args({"-l"})
  assert(opts.list == true, "expected list true")
end
test_parse_args_list_short()

local function test_parse_args_help()
  local opts = main.parse_args({"--help"})
  assert(opts.help == true, "expected help true")
end
test_parse_args_help()

local function test_parse_args_help_short()
  local opts = main.parse_args({"-h"})
  assert(opts.help == true, "expected help true")
end
test_parse_args_help_short()

local function test_parse_args_combined()
  local opts = main.parse_args({"-l", "-o", "/tmp/combo.tar"})
  assert(opts.list == true, "expected list true")
  assert(opts.output == "/tmp/combo.tar", "expected output /tmp/combo.tar")
end
test_parse_args_combined()

--- unit tests: usage

local function test_usage()
  local s = main.usage()
  assert(s:find("--output"), "expected --output in usage")
  assert(s:find("--list"), "expected --list in usage")
  assert(s:find("--help"), "expected --help in usage")
  assert(s:find("tar xf"), "expected tar example in usage")
end
test_usage()

--- unit tests: tar

local function test_tar_from_file()
  local content, err = main.tar("o/world.tar")
  assert(content, "expected tarball content, got error: " .. tostring(err))
  assert(#content > 0, "expected non-empty tarball")
end
test_tar_from_file()

local function test_tar_nonexistent()
  local content, err = main.tar("/no/such/file.tar")
  assert(content == nil, "expected nil for nonexistent tar")
  assert(err, "expected error message")
end
test_tar_nonexistent()

--- integration tests: run the world binary

local function test_cli_help()
  local h = spawn.spawn({world, "--help"})
  local code = h:wait()
  assert(code == 0, "world --help exited " .. tostring(code))
end
test_cli_help()

local function test_cli_list()
  local h = spawn.spawn({world, "--list"})
  local ok, out = h:read()
  assert(ok, "world --list failed: " .. tostring(out))
  assert((out as string):find("config"), "expected config in list output")
end
test_cli_list()

local function test_cli_stdout()
  local tmpdir = env.get("TEST_TMPDIR")
  assert(tmpdir, "TEST_TMPDIR must be set")

  local h = spawn.spawn({world})
  local ok, out = h:read()
  assert(ok, "world stdout failed: " .. tostring(out))
  local tar_data = out as string
  assert(#tar_data > 0, "expected non-empty stdout")

  -- verify it's a valid tarball by extracting
  local dest = fs.join(tmpdir, "stdout_extract")
  fs.makedirs(dest)
  h = spawn.spawn({"tar", "xf", "-", "-C", dest}, {stdin = tar_data})
  local _, _, code = h:read()
  assert(code == 0, "tar extract failed with code " .. tostring(code))
  assert(fs.isfile(fs.join(dest, ".config/example/config")),
    "expected config file after extraction")
end
test_cli_stdout()

local function test_cli_output_file()
  local tmpdir = env.get("TEST_TMPDIR")
  assert(tmpdir, "TEST_TMPDIR must be set")
  local tarpath = fs.join(tmpdir, "out.tar")

  local h = spawn.spawn({world, "-o", tarpath})
  local _, _, code = h:read()
  assert(code == 0, "world -o failed with code " .. tostring(code))
  assert(fs.isfile(tarpath), "expected tar file at " .. tarpath)

  -- verify contents
  local dest = fs.join(tmpdir, "file_extract")
  fs.makedirs(dest)
  h = spawn.spawn({"tar", "xf", tarpath, "-C", dest})
  local _ok2, _out2, code2 = h:read()
  assert(code2 == 0, "tar extract failed with code " .. tostring(code2))
  assert(fs.isfile(fs.join(dest, ".config/example/config")),
    "expected config file after extraction")
end
test_cli_output_file()
