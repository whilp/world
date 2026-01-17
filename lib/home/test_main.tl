#!/usr/bin/env run-test.lua
-- ast-grep ignore: test file uses temp path concatenation

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local home = require("home.main")

local TEST_TMPDIR = os.getenv("TEST_TMPDIR") or "/tmp"
local TEST_BIN = os.getenv("TEST_BIN") or ""

local record Args
  cmd: string
  dest: string
  force: boolean
end

local record CapturedOutput
  stderr: string
  stdout: string
  stderr_obj: FILE
  stdout_obj: FILE
end

local record Manifest
  files: {string:any}
  version: string
end

local record SpawnHandle
  read: function(self: SpawnHandle): boolean, string
  wait: function(self: SpawnHandle): integer
end

local function capture_output(): CapturedOutput
  local out: CapturedOutput = { stderr = "", stdout = "" }
  out.stderr_obj = {
    write = function(_: any, s: string): boolean out.stderr = out.stderr .. s return true end,
  } as FILE
  out.stdout_obj = {
    write = function(_: any, s: string): boolean out.stdout = out.stdout .. s return true end,
  } as FILE
  return out
end

-- Test built binary has valid manifest
local home_bin = path.join(TEST_BIN, "home")
local ok, manifest_content = (spawn({"unzip", "-p", home_bin, "manifest.lua"}) as SpawnHandle):read()
assert(ok, "failed to extract manifest.lua from built binary")
assert(manifest_content and #manifest_content > 0, "manifest.lua is empty in built binary")
local chunk, err = load(manifest_content)
assert(chunk, "manifest.lua is not valid lua: " .. tostring(err))
local manifest = chunk() as Manifest
assert(type(manifest) == "table", "manifest.lua does not return a table")
assert(manifest.files, "manifest.lua missing files key")
assert(next(manifest.files), "manifest.lua has empty files")

-- Test built binary has dotfiles.zip
ok, _ = (spawn({"unzip", "-p", home_bin, "dotfiles.zip"}) as SpawnHandle):read()
assert(ok, "failed to extract dotfiles.zip from built binary")

-- Test parse_args
local args = home.parse_args({ "unpack", "/tmp/dest" }) as Args
assert(args.cmd == "unpack", "parse_args cmd")
assert(args.dest == "/tmp/dest", "parse_args dest")
assert(not args.force, "parse_args force default")

args = home.parse_args({ "unpack", "--force", "/tmp/dest" }) as Args
assert(args.force, "parse_args --force")

args = home.parse_args({}) as Args
assert(args.cmd == "help", "parse_args default help")

-- Test copy_file
local src = path.join(TEST_TMPDIR, "source.txt")
local dst = path.join(TEST_TMPDIR, "dest.txt")
cosmo.Barf(src, "hello world")

local copy_ok, copy_err = home.copy_file(src, dst)
assert(copy_ok, "copy_file: " .. tostring(copy_err))

local f = io.open(dst, "r")
assert(f, "copy_file destination exists")
local content = f:read("*a")
f:close()
assert(content == "hello world", "copy_file content")

-- Test copy_file no overwrite
cosmo.Barf(dst, "existing")
copy_ok, copy_err = home.copy_file(src, dst, nil, false)
assert(not copy_ok, "copy_file should fail without overwrite")
assert(copy_err:find("already exists"), "copy_file error message")

-- Test copy_file with overwrite
copy_ok, copy_err = home.copy_file(src, dst, nil, true)
assert(copy_ok, "copy_file overwrite: " .. tostring(copy_err))

-- Test read_file
local data: string
data, err = home.read_file(src)
assert(data == "hello world", "read_file content")

data, err = home.read_file("/nonexistent/path")
assert(not data, "read_file nonexistent should fail")

-- Test format_mode
assert(home.format_mode(tonumber("644", 8), false) == "-rw-r--r--", "format_mode 644")
assert(home.format_mode(tonumber("755", 8), false) == "-rwxr-xr-x", "format_mode 755")
assert(home.format_mode(tonumber("755", 8), true) == "drwxr-xr-x", "format_mode dir")

-- Find system unzip
local unzip_bin = "/usr/bin/unzip"
local unzip_stat = unix.stat(unzip_bin)
if not unzip_stat then
  unzip_bin = path.join(os.getenv("HOME") or "", ".local", "bin", "unzip")
end

-- Test cmd_unpack with missing dotfiles.zip
local out = capture_output()
local ret = home.cmd_unpack(path.join(TEST_TMPDIR, "unpack_missing"), false, {
  stderr = out.stderr_obj,
  stdout = out.stdout_obj,
  dotfiles_zip = "/nonexistent/dotfiles.zip",
  unzip_bin = unzip_bin,
})
assert(ret == 1, "cmd_unpack missing dotfiles.zip should return 1")
assert(out.stderr:find("not found"), "missing dotfiles.zip error message")

-- Test cmd_unpack with symlink preservation
-- Create a test zip with a symlink
local test_zip_dir = path.join(TEST_TMPDIR, "test_zip_contents")
local test_zip = path.join(TEST_TMPDIR, "test_dotfiles.zip")
local extract_dir = path.join(TEST_TMPDIR, "extracted")

unix.rmrf(test_zip_dir)
unix.rmrf(extract_dir)
unix.makedirs(test_zip_dir)
unix.makedirs(path.join(test_zip_dir, ".local", "bin"))

-- Create a file and symlink
cosmo.Barf(path.join(test_zip_dir, ".local", "bin", "real-binary"), "binary content")
unix.symlink("real-binary", path.join(test_zip_dir, ".local", "bin", "link-to-binary"))

-- Create zip with symlinks preserved
unix.unlink(test_zip)  -- Remove if exists
local orig_dir = unix.getcwd()
unix.chdir(test_zip_dir)
local zip_result = (spawn({"zip", "-qry", test_zip, "."}) as SpawnHandle):wait()
unix.chdir(orig_dir)
assert(zip_result == 0, "failed to create test zip")

-- Extract using cmd_unpack
out = capture_output()
ret = home.cmd_unpack(extract_dir, false, {
  stderr = out.stderr_obj,
  stdout = out.stdout_obj,
  dotfiles_zip = test_zip,
  unzip_bin = unzip_bin,
  tools_dir = "/nonexistent",  -- No tools to copy
})
assert(ret == 0, "cmd_unpack should succeed, got: " .. ret .. " stderr: " .. out.stderr)

-- Verify symlink was preserved
local link_path = path.join(extract_dir, ".local", "bin", "link-to-binary")
local link_stat = unix.stat(link_path, unix.AT_SYMLINK_NOFOLLOW)
assert(link_stat, "symlink should exist at " .. link_path)
assert(unix.S_ISLNK(link_stat:mode()), "link-to-binary should be a symlink")

-- Verify symlink target
local link_target = unix.readlink(link_path, 256)
assert(link_target == "real-binary", "symlink target should be 'real-binary', got: " .. tostring(link_target))

-- Test cmd_list with manifest from built binary
out = capture_output()
ret = home.cmd_list({
  manifest = manifest,
  stderr = out.stderr_obj,
  stdout = out.stdout_obj,
})
assert(ret == 0, "cmd_list with valid manifest should return 0")
assert(#out.stdout > 0, "cmd_list should produce output")

-- Test unknown flags are rejected
local handle = spawn({home_bin, "unpack", "--with-platform", TEST_TMPDIR})
local exit_code = handle:wait()
assert(exit_code ~= 0, "unknown flag --with-platform should fail, got exit code: " .. tostring(exit_code))

handle = spawn({home_bin, "unpack", "--unknown-flag", TEST_TMPDIR})
exit_code = handle:wait()
assert(exit_code ~= 0, "unknown flag --unknown-flag should fail")

-- Test dotfiles.zip coverage: all git-tracked files should be in the zip
-- Get git files (excluding build artifacts)
local git_ok, git_output = (spawn({"git", "ls-files"}) as SpawnHandle):read()
assert(git_ok, "failed to run git ls-files")

local git_files: {string:boolean} = {}
for line in (git_output or ""):gmatch("[^\n]+") do
  -- Exclude 3p/, o/, results/, Makefile, .git, world/ (submodule)
  if not line:match("^3p/") and not line:match("^o/") and not line:match("^results/")
     and not line:match("^Makefile") and not line:match("^%.git")
     and not line:match("^world/") then
    git_files[line] = true
  end
end

-- Get zip files
local zip_ok, zip_output = (spawn({"unzip", "-Z1", home_bin, "dotfiles.zip"}) as SpawnHandle):read()
-- Extract dotfiles.zip to temp and list its contents
local dotfiles_tmp = path.join(TEST_TMPDIR, "dotfiles_coverage")
unix.makedirs(dotfiles_tmp)
local unzip_result = (spawn({"unzip", "-q", "-o", home_bin, "dotfiles.zip", "-d", dotfiles_tmp}) as SpawnHandle):wait()
assert(unzip_result == 0, "failed to extract dotfiles.zip from home binary")

local list_ok, list_output = (spawn({"unzip", "-Z1", path.join(dotfiles_tmp, "dotfiles.zip")}) as SpawnHandle):read()
assert(list_ok, "failed to list dotfiles.zip contents")

local zip_files: {string:boolean} = {}
for line in (list_output or ""):gmatch("[^\n]+") do
  if not line:match("/$") then  -- Skip directories
    zip_files[line] = true
  end
end

-- Check coverage: every git file should be in zip (with some exceptions)
local missing: {string} = {}
for f in pairs(git_files) do
  local in_zip = zip_files[f]
  -- .tl files in .config/nvim are compiled to .lua
  if not in_zip and f:match("^%.config/nvim/.*%.tl$") then
    local lua_version = f:gsub("%.tl$", ".lua")
    in_zip = zip_files[lua_version]
  end
  if not in_zip then
    table.insert(missing, f)
  end
end

unix.rmrf(dotfiles_tmp)

if #missing > 0 then
  table.sort(missing)
  local msg = "dotfiles.zip missing " .. #missing .. " git-tracked files:\n"
  for i = 1, math.min(20, #missing) do
    msg = msg .. "  " .. missing[i] .. "\n"
  end
  if #missing > 20 then
    msg = msg .. "  ... and " .. (#missing - 20) .. " more\n"
  end
  msg = msg .. "Add wildcards to dots_* in lib/home/cook.mk"
  assert(false, msg)
end
