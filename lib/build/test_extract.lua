local lu = require("luaunit")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("spawn").spawn
local cosmo = require("cosmo")

local extract = dofile("lib/build/extract.lua")

local function file_exists(filepath)
  return unix.stat(filepath) ~= nil
end

local function write_file(filepath, content)
  unix.makedirs(path.dirname(filepath))
  cosmo.Barf(filepath, content or "test", tonumber("644", 8))
end

local function create_test_zip_with_wrapper(name)
  local archive_dir = path.join(TEST_TMPDIR, "archive_src")
  local archive_path = path.join(TEST_TMPDIR, name)

  unix.rmrf(archive_dir)
  unix.makedirs(archive_dir)

  local wrapper_dir = path.join(archive_dir, "wrapper-1.0")
  unix.makedirs(wrapper_dir)
  write_file(path.join(wrapper_dir, "file1.txt"), "content1")
  write_file(path.join(wrapper_dir, "file2.txt"), "content2")

  local cwd = unix.getcwd()
  unix.chdir(archive_dir)
  local handle = spawn({"zip", "-r", archive_path, "wrapper-1.0"})
  local exit_code = handle:wait()
  unix.chdir(cwd)
  lu.assertEquals(exit_code, 0, "failed to create test zip")

  unix.rmrf(archive_dir)
  return archive_path
end

local function create_test_zip_no_wrapper(name)
  local archive_dir = path.join(TEST_TMPDIR, "archive_src")
  local archive_path = path.join(TEST_TMPDIR, name)

  unix.rmrf(archive_dir)
  unix.makedirs(archive_dir)

  write_file(path.join(archive_dir, "file1.txt"), "content1")
  write_file(path.join(archive_dir, "file2.txt"), "content2")

  local cwd = unix.getcwd()
  unix.chdir(archive_dir)
  local handle = spawn({"zip", "-r", archive_path, "file1.txt", "file2.txt"})
  local exit_code = handle:wait()
  unix.chdir(cwd)
  lu.assertEquals(exit_code, 0, "failed to create test zip")

  unix.rmrf(archive_dir)
  return archive_path
end

local function create_test_targz_with_wrapper(name)
  local archive_dir = path.join(TEST_TMPDIR, "archive_src")
  local archive_path = path.join(TEST_TMPDIR, name)

  unix.rmrf(archive_dir)
  unix.makedirs(archive_dir)

  local wrapper_dir = path.join(archive_dir, "wrapper-1.0")
  unix.makedirs(wrapper_dir)
  write_file(path.join(wrapper_dir, "file1.txt"), "content1")
  write_file(path.join(wrapper_dir, "file2.txt"), "content2")

  local cwd = unix.getcwd()
  unix.chdir(archive_dir)
  local handle = spawn({"tar", "-czf", archive_path, "wrapper-1.0"})
  local exit_code = handle:wait()
  unix.chdir(cwd)
  lu.assertEquals(exit_code, 0, "failed to create test tar.gz")

  unix.rmrf(archive_dir)
  return archive_path
end

local function create_test_targz_no_wrapper(name)
  local archive_dir = path.join(TEST_TMPDIR, "archive_src")
  local archive_path = path.join(TEST_TMPDIR, name)

  unix.rmrf(archive_dir)
  unix.makedirs(archive_dir)

  write_file(path.join(archive_dir, "file1.txt"), "content1")
  write_file(path.join(archive_dir, "file2.txt"), "content2")

  local cwd = unix.getcwd()
  unix.chdir(archive_dir)
  local handle = spawn({"tar", "-czf", archive_path, "file1.txt", "file2.txt"})
  local exit_code = handle:wait()
  unix.chdir(cwd)
  lu.assertEquals(exit_code, 0, "failed to create test tar.gz")

  unix.rmrf(archive_dir)
  return archive_path
end

TestZipExtractNoStrip = {}

function TestZipExtractNoStrip:setUp()
  self.archive = create_test_zip_no_wrapper("test_no_strip.zip")
  self.dest = path.join(TEST_TMPDIR, "dest_no_strip")
  unix.rmrf(self.dest)
  unix.makedirs(self.dest)
end

function TestZipExtractNoStrip:tearDown()
  unix.rmrf(self.dest)
  unix.rmrf(self.archive)
end

function TestZipExtractNoStrip:test_extracts_directly()
  local ok, err = extract.extract_zip(self.archive, self.dest, 0)

  lu.assertTrue(ok, "extract should succeed: " .. tostring(err))
  lu.assertTrue(file_exists(path.join(self.dest, "file1.txt")))
  lu.assertTrue(file_exists(path.join(self.dest, "file2.txt")))
end

TestZipExtractWithStrip = {}

function TestZipExtractWithStrip:setUp()
  self.archive = create_test_zip_with_wrapper("test_with_strip.zip")
  self.dest = path.join(TEST_TMPDIR, "dest_with_strip")
  unix.rmrf(self.dest)
  unix.makedirs(self.dest)
end

function TestZipExtractWithStrip:tearDown()
  unix.rmrf(self.dest)
  unix.rmrf(self.archive)
end

function TestZipExtractWithStrip:test_strips_wrapper_directory()
  local ok, err = extract.extract_zip(self.archive, self.dest, 1)

  lu.assertTrue(ok, "extract should succeed: " .. tostring(err))
  lu.assertTrue(file_exists(path.join(self.dest, "file1.txt")))
  lu.assertTrue(file_exists(path.join(self.dest, "file2.txt")))
  lu.assertFalse(file_exists(path.join(self.dest, "wrapper-1.0")))
end

TestTarGzExtractNoStrip = {}

function TestTarGzExtractNoStrip:setUp()
  self.archive = create_test_targz_no_wrapper("test_no_strip.tar.gz")
  self.dest = path.join(TEST_TMPDIR, "dest_targz_no_strip")
  unix.rmrf(self.dest)
  unix.makedirs(self.dest)
end

function TestTarGzExtractNoStrip:tearDown()
  unix.rmrf(self.dest)
  unix.rmrf(self.archive)
end

function TestTarGzExtractNoStrip:test_extracts_directly()
  local ok, err = extract.extract_targz(self.archive, self.dest, 0)

  lu.assertTrue(ok, "extract should succeed: " .. tostring(err))
  lu.assertTrue(file_exists(path.join(self.dest, "file1.txt")))
  lu.assertTrue(file_exists(path.join(self.dest, "file2.txt")))
end

TestTarGzExtractWithStrip = {}

function TestTarGzExtractWithStrip:setUp()
  self.archive = create_test_targz_with_wrapper("test_with_strip.tar.gz")
  self.dest = path.join(TEST_TMPDIR, "dest_targz_with_strip")
  unix.rmrf(self.dest)
  unix.makedirs(self.dest)
end

function TestTarGzExtractWithStrip:tearDown()
  unix.rmrf(self.dest)
  unix.rmrf(self.archive)
end

function TestTarGzExtractWithStrip:test_strips_wrapper_directory()
  local ok, err = extract.extract_targz(self.archive, self.dest, 1)

  lu.assertTrue(ok, "extract should succeed: " .. tostring(err))
  lu.assertTrue(file_exists(path.join(self.dest, "file1.txt")))
  lu.assertTrue(file_exists(path.join(self.dest, "file2.txt")))
  lu.assertFalse(file_exists(path.join(self.dest, "wrapper-1.0")))
end

TestStripComponentsErrors = {}

function TestStripComponentsErrors:setUp()
  self.archive = create_test_zip_with_wrapper("test_error.zip")
  self.dest = path.join(TEST_TMPDIR, "dest_error")
  unix.rmrf(self.dest)
  unix.makedirs(self.dest)
end

function TestStripComponentsErrors:tearDown()
  unix.rmrf(self.dest)
  unix.rmrf(self.archive)
end

function TestStripComponentsErrors:test_too_many_components_fails()
  local ok = extract.extract_zip(self.archive, self.dest, 3)

  lu.assertNil(ok, "extract should fail when stripping too many components")
end

-- Test strip_components=2 where final entry is a file (like superhtml archives)
TestStripComponentsDeepFile = {}

function TestStripComponentsDeepFile:setUp()
  -- create archive with structure: wrapper/subdir/binary
  local archive_dir = path.join(TEST_TMPDIR, "archive_deep")
  self.archive = path.join(TEST_TMPDIR, "test_deep.tar.gz")
  self.dest = path.join(TEST_TMPDIR, "dest_deep")

  unix.rmrf(archive_dir)
  unix.rmrf(self.dest)
  unix.makedirs(self.dest)

  local deep_dir = path.join(archive_dir, "wrapper/subdir")
  unix.makedirs(deep_dir)
  write_file(path.join(deep_dir, "binary"), "executable")

  local cwd = unix.getcwd()
  unix.chdir(archive_dir)
  local handle = spawn({"tar", "-czf", self.archive, "wrapper"})
  local exit_code = handle:wait()
  unix.chdir(cwd)
  lu.assertEquals(exit_code, 0, "failed to create test tar.gz")

  unix.rmrf(archive_dir)
end

function TestStripComponentsDeepFile:tearDown()
  unix.rmrf(self.dest)
  unix.rmrf(self.archive)
end

function TestStripComponentsDeepFile:test_strips_to_file()
  local ok, err = extract.extract_targz(self.archive, self.dest, 2)

  lu.assertTrue(ok, "extract should succeed: " .. tostring(err))
  lu.assertTrue(file_exists(path.join(self.dest, "binary")))
end
