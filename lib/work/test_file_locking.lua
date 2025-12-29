local lu = require("luaunit")

-- skip if posix not available (work module requires luaposix)
local has_posix = pcall(require, "posix")
if not has_posix then
  function test_file_locking_skipped()
    lu.skip("requires luaposix")
  end
  os.exit(lu.LuaUnit.run())
end

local unix = require("cosmo.unix")
local path = require("cosmo.path")

local data = require("work.data")
local store = require("work.store")
local Work = require("work.test_lib")
local test_store = Work.store

local function remove_dir(dir_path)
  for name in unix.opendir(dir_path) do
    if name ~= "." and name ~= ".." then
      local full_path = path.join(dir_path, name)
      local st = unix.stat(full_path, unix.AT_SYMLINK_NOFOLLOW)
      if st and unix.S_ISDIR(st:mode()) then
        remove_dir(full_path)
      else
        unix.unlink(full_path)
      end
    end
  end
  unix.rmdir(dir_path)
end

TestFileLocking = {}

function TestFileLocking:setUp()
  store.reset(test_store)
  self.test_dir = "/tmp/work-test-" .. os.time()
  unix.makedirs(self.test_dir)
end

function TestFileLocking:tearDown()
  data.release_lock()
  remove_dir(self.test_dir)
  data._lock_handle = nil
  data._lock_path = nil
end

function TestFileLocking:test_acquire_and_release_lock()
  local ok, err = data.acquire_lock(self.test_dir)
  lu.assertTrue(ok, "should acquire lock successfully: " .. tostring(err))
  lu.assertNotNil(data._lock_handle, "lock handle should be set")
  lu.assertEquals(data._lock_path, self.test_dir .. "/.work.lock")

  ok = data.release_lock()
  lu.assertTrue(ok, "should release lock successfully")
  lu.assertNil(data._lock_handle, "lock handle should be cleared")
  lu.assertNil(data._lock_path, "lock path should be cleared")
end

function TestFileLocking:test_lock_creates_lock_file()
  local ok = data.acquire_lock(self.test_dir)
  lu.assertTrue(ok, "first lock should succeed")

  -- Verify lock file exists
  local lock_path = self.test_dir .. "/.work.lock"
  local f = io.open(lock_path, "r")
  lu.assertNotNil(f, "lock file should exist")
  f:close()

  data.release_lock()

  -- Lock file should still exist after release (but unlocked)
  f = io.open(lock_path, "r")
  lu.assertNotNil(f, "lock file should still exist after release")
  f:close()
end

function TestFileLocking:test_save_with_locking()
  local item = {
    id = "01TEST0000000000000000001",
    title = "test item",
    created = "2025-12-01",
  }

  local ok, err = data.save(item, self.test_dir)
  lu.assertTrue(ok, "save should succeed: " .. tostring(err))

  -- Verify lock is released after save
  lu.assertNil(data._lock_handle, "lock should be released after save")

  -- Verify file was created
  local file_path = self.test_dir .. "/01TEST0000000000000000001.lua"
  local f = io.open(file_path, "r")
  lu.assertNotNil(f, "work item file should exist")
  f:close()
end

function TestFileLocking:test_save_releases_lock_on_error()
  local item = {
    id = "01TEST0000000000000000001",
    title = 123,  -- invalid type
    created = "2025-12-01",
  }

  local ok, err = data.save(item, self.test_dir)
  lu.assertNil(ok, "save should fail due to validation error")
  lu.assertNotNil(err, "should return error message")

  -- Verify lock is released even on error
  lu.assertNil(data._lock_handle, "lock should be released after error")
end

function TestFileLocking:test_delete_with_locking()
  local item = {
    id = "01TEST0000000000000000002",
    title = "test delete",
    created = "2025-12-01",
  }

  -- Save the item first
  local ok, err = data.save(item, self.test_dir)
  lu.assertTrue(ok, "save should succeed: " .. tostring(err))

  -- Load it to get the _meta.source
  store.reset(test_store)
  data.load_all(test_store, self.test_dir)
  local loaded_item = data.get(test_store, "01TEST0000000000000000002")
  lu.assertNotNil(loaded_item, "item should be loaded")

  -- Delete it
  ok, err = data.delete(loaded_item, self.test_dir)
  lu.assertTrue(ok, "delete should succeed: " .. tostring(err))

  -- Verify lock is released after delete
  lu.assertNil(data._lock_handle, "lock should be released after delete")

  -- Verify file is gone
  local file_path = self.test_dir .. "/01TEST0000000000000000002.lua"
  local f = io.open(file_path, "r")
  lu.assertNil(f, "work item file should not exist after delete")
end

os.exit(lu.LuaUnit.run())
