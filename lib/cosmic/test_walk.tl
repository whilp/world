#!/usr/bin/env run-test.lua
local walk = require("cosmic.walk")
local unix = require("cosmo.unix")
local path = require("cosmo.path")

global TEST_TMPDIR: string

local record Stat
  mode: function(self): number
  size: function(self): number
  mtim: function(self): number
end

local record WalkContext
  count: number
  dirs: number
end

local function setup(): string
  local test_dir = path.join(TEST_TMPDIR, "walk_test")
  unix.makedirs(path.join(test_dir, "subdir"))
  unix.makedirs(path.join(test_dir, "subdir/nested"))

  local function touch(filepath: string)
    local fd = unix.open(filepath, unix.O_WRONLY | unix.O_CREAT, tonumber("644", 8))
    unix.close(fd)
  end

  touch(path.join(test_dir, "file1.lua"))
  touch(path.join(test_dir, "file2.txt"))
  touch(path.join(test_dir, "subdir/file3.lua"))
  touch(path.join(test_dir, "subdir/nested/file4.lua"))

  return test_dir
end

local function teardown(test_dir: string)
  if test_dir then
    unix.rmrf(test_dir)
  end
end

local function test_collect_finds_lua_files()
  local test_dir = setup()
  local files = walk.collect(test_dir, "%.lua$")

  assert(#files == 3, "expected 3 lua files, got " .. #files)

  local found: {string:boolean} = {}
  for _, f in ipairs(files) do
    local name = path.basename(f)
    found[name] = true
  end

  assert(found["file1.lua"], "expected file1.lua to be found")
  assert(found["file3.lua"], "expected file3.lua to be found")
  assert(found["file4.lua"], "expected file4.lua to be found")
  assert(found["file2.txt"] == nil, "expected file2.txt not to be found")
  teardown(test_dir)
end
test_collect_finds_lua_files()

local function test_collect_finds_nested_files()
  local test_dir = setup()
  local files = walk.collect(test_dir, "%.txt$")
  assert(#files == 1, "expected 1 txt file, got " .. #files)
  assert(path.basename(files[1]) == "file2.txt", "expected file2.txt, got " .. path.basename(files[1]))
  teardown(test_dir)
end
test_collect_finds_nested_files()

local function test_walk_with_visitor()
  local test_dir = setup()
  local ctx: WalkContext = { count = 0, dirs = 0 }

  walk.walk(test_dir, function(_full_path: string, _entry: string, stat: Stat, c: WalkContext): boolean
    c.count = c.count + 1
    if unix.S_ISDIR(stat:mode()) then
      c.dirs = c.dirs + 1
    end
    return true
  end, ctx)

  assert(ctx.count > 0, "expected count > 0")
  assert(ctx.dirs == 2, "expected 2 dirs, got " .. ctx.dirs)
  teardown(test_dir)
end
test_walk_with_visitor()

local function test_collect_all()
  local test_dir = setup()
  local files = walk.collect_all(test_dir)

  assert(files["file1.lua"] ~= nil, "expected file1.lua in results")
  assert(files["file2.txt"] ~= nil, "expected file2.txt in results")
  assert(files["subdir/file3.lua"] ~= nil, "expected subdir/file3.lua in results")
  assert(files["subdir/nested/file4.lua"] ~= nil, "expected subdir/nested/file4.lua in results")

  assert(files["file1.lua"].mode ~= nil, "expected mode property on file1.lua")
  assert(files["subdir"] == nil, "expected subdir not to be in results")
  teardown(test_dir)
end
test_collect_all()

local function test_walk_empty_directory()
  local empty_dir = path.join(TEST_TMPDIR, "walk_empty")
  unix.makedirs(empty_dir)
  local files = walk.collect(empty_dir, "%.lua$")
  assert(#files == 0, "expected 0 files, got " .. #files)
  unix.rmrf(empty_dir)
end
test_walk_empty_directory()
