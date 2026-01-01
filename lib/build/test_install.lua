local lu = require("luaunit")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local install = require("build.install")

local tmp_dir = "/tmp/test_install_" .. os.time()

local function write_file(filepath, content)
  unix.makedirs(path.dirname(filepath))
  local f = io.open(filepath, "w")
  f:write(content or "test")
  f:close()
  unix.chmod(filepath, tonumber("755", 8))
end

local function file_exists(filepath)
  return unix.stat(filepath) ~= nil
end

TestCopyFile = {}

function TestCopyFile:setUp()
  unix.makedirs(path.join(tmp_dir, "target"))
end

function TestCopyFile:tearDown()
  unix.rmrf(tmp_dir)
end

-- generic names like "download" should be renamed to tool name
function TestCopyFile:test_generic_name_renamed()
  local source = path.join(tmp_dir, "download")
  local target_dir = path.join(tmp_dir, "target")

  write_file(source)

  local ok, err = install.copy_file(source, target_dir, "bin", "mytool")
  lu.assertNil(err)
  lu.assertTrue(ok)
  lu.assertTrue(file_exists(path.join(target_dir, "mytool")))
  lu.assertFalse(file_exists(path.join(target_dir, "download")))
end

-- specific names like "lua" should be preserved
function TestCopyFile:test_specific_name_preserved()
  local source = path.join(tmp_dir, "lua")
  local target_dir = path.join(tmp_dir, "target")

  write_file(source)

  local ok, err = install.copy_file(source, target_dir, "bin", "cosmos")
  lu.assertNil(err)
  lu.assertTrue(ok)
  lu.assertTrue(file_exists(path.join(target_dir, "lua")))
  lu.assertFalse(file_exists(path.join(target_dir, "cosmos")))
end

-- lib files should never be renamed
function TestCopyFile:test_lib_never_renamed()
  local source = path.join(tmp_dir, "download")
  local target_dir = path.join(tmp_dir, "target")

  write_file(source)

  local ok, err = install.copy_file(source, target_dir, "lib", "mytool")
  lu.assertNil(err)
  lu.assertTrue(ok)
  lu.assertTrue(file_exists(path.join(target_dir, "download")))
  lu.assertFalse(file_exists(path.join(target_dir, "mytool")))
end

-- "binary" is also a generic name
function TestCopyFile:test_binary_name_renamed()
  local source = path.join(tmp_dir, "binary")
  local target_dir = path.join(tmp_dir, "target")

  write_file(source)

  local ok, err = install.copy_file(source, target_dir, "bin", "myapp")
  lu.assertNil(err)
  lu.assertTrue(ok)
  lu.assertTrue(file_exists(path.join(target_dir, "myapp")))
end

-- real tool names should be preserved
function TestCopyFile:test_zip_name_preserved()
  local source = path.join(tmp_dir, "zip")
  local target_dir = path.join(tmp_dir, "target")

  write_file(source)

  local ok, err = install.copy_file(source, target_dir, "bin", "cosmos")
  lu.assertNil(err)
  lu.assertTrue(ok)
  lu.assertTrue(file_exists(path.join(target_dir, "zip")))
end

-- test runner handles execution
