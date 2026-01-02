local walk_lib = require("walk")
local unix = require("cosmo.unix")
local path = require("cosmo.path")

local TestWalk = {}

function TestWalk:setup()
  self.test_dir = unix.mkdtemp("/tmp/walk_test_XXXXXX")
  unix.makedirs(path.join(self.test_dir, "subdir"))
  unix.makedirs(path.join(self.test_dir, "subdir/nested"))

  local function touch(filepath)
    local fd = unix.open(filepath, unix.O_WRONLY | unix.O_CREAT, tonumber("644", 8))
    unix.close(fd)
  end

  touch(path.join(self.test_dir, "file1.lua"))
  touch(path.join(self.test_dir, "file2.txt"))
  touch(path.join(self.test_dir, "subdir/file3.lua"))
  touch(path.join(self.test_dir, "subdir/nested/file4.lua"))
end

function TestWalk:teardown()
  if self.test_dir then
    unix.rmrf(self.test_dir)
  end
end

function TestWalk:test_collect_finds_lua_files()
  local files = walk_lib.collect(self.test_dir, "%.lua$")

  assert(#files == 3, "expected 3 lua files, got " .. #files)

  local found = {}
  for _, f in ipairs(files) do
    local name = path.basename(f)
    found[name] = true
  end

  assert(found["file1.lua"], "missing file1.lua")
  assert(found["file3.lua"], "missing file3.lua")
  assert(found["file4.lua"], "missing file4.lua")
  assert(not found["file2.txt"], "should not find txt file")
end

function TestWalk:test_collect_finds_nested_files()
  local files = walk_lib.collect(self.test_dir, "%.txt$")
  assert(#files == 1, "expected 1 txt file, got " .. #files)
  assert(path.basename(files[1]) == "file2.txt")
end

function TestWalk:test_walk_with_visitor()
  local count = 0
  local dirs = 0

  walk_lib.walk(self.test_dir, function(full_path, entry, stat, ctx)
    ctx.count = ctx.count + 1
    if unix.S_ISDIR(stat:mode()) then
      ctx.dirs = ctx.dirs + 1
    end
  end, { count = 0, dirs = 0 })

  -- should visit 4 files + 2 directories (subdir, nested)
  -- note: visitor is called for everything including dirs
end

function TestWalk:test_collect_all()
  local files = walk_lib.collect_all(self.test_dir)

  assert(files["file1.lua"], "missing file1.lua")
  assert(files["file2.txt"], "missing file2.txt")
  assert(files["subdir/file3.lua"], "missing subdir/file3.lua")
  assert(files["subdir/nested/file4.lua"], "missing subdir/nested/file4.lua")

  assert(files["file1.lua"].mode, "file1.lua should have mode")
  assert(not files["subdir"], "directories should not be in results")
end

function TestWalk:test_walk_empty_directory()
  local empty_dir = unix.mkdtemp("/tmp/walk_empty_XXXXXX")
  local files = walk_lib.collect(empty_dir, "%.lua$")
  assert(#files == 0, "empty directory should return 0 files")
  unix.rmrf(empty_dir)
end

local function run_tests()
  local tests = {}
  for name, func in pairs(TestWalk) do
    if name:match("^test_") then
      table.insert(tests, { name = name, func = func })
    end
  end

  table.sort(tests, function(a, b) return a.name < b.name end)

  local passed = 0
  local failed = 0

  for _, test in ipairs(tests) do
    TestWalk:setup()
    local ok, err = pcall(test.func, TestWalk)
    TestWalk:teardown()

    if ok then
      print("✓ " .. test.name)
      passed = passed + 1
    else
      print("✗ " .. test.name)
      print("  " .. tostring(err))
      failed = failed + 1
    end
  end

  print("")
  print(string.format("passed: %d, failed: %d", passed, failed))
  return failed == 0
end

if not pcall(debug.getlocal, 4, 1) then
  os.exit(run_tests() and 0 or 1)
end

return TestWalk
