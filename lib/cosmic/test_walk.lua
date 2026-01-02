local lu = require("luaunit")
local walk = require("cosmic.walk")
local unix = require("cosmo.unix")
local path = require("cosmo.path")

TestWalk = {}

function TestWalk:setUp()
  self.test_dir = path.join(TEST_TMPDIR, "walk_test")
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

function TestWalk:tearDown()
  if self.test_dir then
    unix.rmrf(self.test_dir)
  end
end

function TestWalk:test_collect_finds_lua_files()
  local files = walk.collect(self.test_dir, "%.lua$")

  lu.assertEquals(#files, 3)

  local found = {}
  for _, f in ipairs(files) do
    local name = path.basename(f)
    found[name] = true
  end

  lu.assertTrue(found["file1.lua"])
  lu.assertTrue(found["file3.lua"])
  lu.assertTrue(found["file4.lua"])
  lu.assertNil(found["file2.txt"])
end

function TestWalk:test_collect_finds_nested_files()
  local files = walk.collect(self.test_dir, "%.txt$")
  lu.assertEquals(#files, 1)
  lu.assertEquals(path.basename(files[1]), "file2.txt")
end

function TestWalk:test_walk_with_visitor()
  local ctx = { count = 0, dirs = 0 }

  walk.walk(self.test_dir, function(_full_path, _entry, stat, c)
    c.count = c.count + 1
    if unix.S_ISDIR(stat:mode()) then
      c.dirs = c.dirs + 1
    end
  end, ctx)

  lu.assertTrue(ctx.count > 0)
  lu.assertEquals(ctx.dirs, 2)
end

function TestWalk:test_collect_all()
  local files = walk.collect_all(self.test_dir)

  lu.assertNotNil(files["file1.lua"])
  lu.assertNotNil(files["file2.txt"])
  lu.assertNotNil(files["subdir/file3.lua"])
  lu.assertNotNil(files["subdir/nested/file4.lua"])

  lu.assertNotNil(files["file1.lua"].mode)
  lu.assertNil(files["subdir"])
end

function TestWalk:test_walk_empty_directory()
  local empty_dir = path.join(TEST_TMPDIR, "walk_empty")
  unix.makedirs(empty_dir)
  local files = walk.collect(empty_dir, "%.lua$")
  lu.assertEquals(#files, 0)
  unix.rmrf(empty_dir)
end

os.exit(lu.LuaUnit.run())
