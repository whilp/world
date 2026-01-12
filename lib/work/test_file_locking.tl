#!/usr/bin/env run-test.lua

-- skip if posix not available (work module requires luaposix)
local has_posix = pcall(require, "posix")
if not has_posix then
  error("SKIP requires luaposix")
end

local record WorkItemMeta
  source: string
  kind: string
end

local record WorkItem
  id: string
  title: string
  created: string
  blocks: {string}
  completed: string
  started: string
  description: string
  due: string
  priority: number
  log: {string:string}
  _meta: WorkItemMeta
end

local record WorkStore
  items: {string:WorkItem}
end

local record DataModule
  _lock_handle: number
  _lock_path: string
  acquire_lock: function(dir: string): boolean, string
  release_lock: function(): boolean, string
  save: function(item: WorkItem, dir: string): boolean, string
  delete: function(item: WorkItem, dir?: string): boolean, string
  load_all: function(store: WorkStore, dir: string): boolean, string
  get: function(store: WorkStore, id: string): WorkItem
end

local record StoreModule
  reset: function(store: WorkStore): WorkStore
end

local record WorkModule
  store: WorkStore
end

local data = require("work.data") as DataModule
local store = require("work.store") as StoreModule
local Work = require("work.test_lib") as WorkModule
local test_store = Work.store

local test_dir = os.getenv("TEST_TMPDIR") as string

local function teardown()
  data.release_lock()
  data._lock_handle = nil
  data._lock_path = nil
end

local function test_acquire_and_release_lock()
  store.reset(test_store)
  local ok, err = data.acquire_lock(test_dir)
  assert(ok, "should acquire lock successfully: " .. tostring(err))
  assert(data._lock_handle ~= nil, "lock handle should be set")
  assert(data._lock_path == test_dir .. "/.work.lock", "expected lock path to match")

  ok = data.release_lock()
  assert(ok, "should release lock successfully")
  assert(data._lock_handle == nil, "lock handle should be cleared")
  assert(data._lock_path == nil, "lock path should be cleared")
  teardown()
end
test_acquire_and_release_lock()

local function test_lock_creates_lock_file()
  store.reset(test_store)
  local ok = data.acquire_lock(test_dir)
  assert(ok, "first lock should succeed")

  -- Verify lock file exists
  local lock_path = test_dir .. "/.work.lock"
  local f = io.open(lock_path, "r")
  assert(f ~= nil, "lock file should exist")
  f:close()

  data.release_lock()

  -- Lock file should still exist after release (but unlocked)
  f = io.open(lock_path, "r")
  assert(f ~= nil, "lock file should still exist after release")
  f:close()
  teardown()
end
test_lock_creates_lock_file()

local function test_save_with_locking()
  store.reset(test_store)
  local item: WorkItem = {
    id = "01TEST0000000000000000001",
    title = "test item",
    created = "2025-12-01",
  }

  local ok, err = data.save(item, test_dir)
  assert(ok, "save should succeed: " .. tostring(err))

  -- Verify lock is released after save
  assert(data._lock_handle == nil, "lock should be released after save")

  -- Verify file was created
  local file_path = test_dir .. "/01TEST0000000000000000001.lua"
  local f = io.open(file_path, "r")
  assert(f ~= nil, "work item file should exist")
  f:close()
  teardown()
end
test_save_with_locking()

local function test_save_releases_lock_on_error()
  store.reset(test_store)
  local item: WorkItem = {
    id = "01TEST0000000000000000001",
    title = 123 as any as string,  -- invalid type
    created = "2025-12-01",
  }

  local ok, err = data.save(item, test_dir)
  assert(ok == nil, "save should fail due to validation error")
  assert(err ~= nil, "should return error message")

  -- Verify lock is released even on error
  assert(data._lock_handle == nil, "lock should be released after error")
  teardown()
end
test_save_releases_lock_on_error()

local function test_delete_with_locking()
  store.reset(test_store)
  local item: WorkItem = {
    id = "01TEST0000000000000000002",
    title = "test delete",
    created = "2025-12-01",
  }

  -- Save the item first
  local ok, err = data.save(item, test_dir)
  assert(ok, "save should succeed: " .. tostring(err))

  -- Load it to get the _meta.source
  store.reset(test_store)
  data.load_all(test_store, test_dir)
  local loaded_item = data.get(test_store, "01TEST0000000000000000002")
  assert(loaded_item ~= nil, "item should be loaded")

  -- Delete it
  ok, err = data.delete(loaded_item, test_dir)
  assert(ok, "delete should succeed: " .. tostring(err))

  -- Verify lock is released after delete
  assert(data._lock_handle == nil, "lock should be released after delete")

  -- Verify file is gone
  local file_path = test_dir .. "/01TEST0000000000000000002.lua"
  local f = io.open(file_path, "r")
  assert(f == nil, "work item file should not exist after delete")
  teardown()
end
test_delete_with_locking()
