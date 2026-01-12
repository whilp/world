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
  validate_string_content: function(str: string, field_name: string): boolean, string
  validate: function(item: any): boolean, string
end

local record StoreModule
  reset: function(store: WorkStore): WorkStore
  add: function(store: WorkStore, item: WorkItem): WorkStore
end

local record WorkModule
  store: WorkStore
end

local data = require("work.data") as DataModule
local store = require("work.store") as StoreModule
local Work = require("work.test_lib") as WorkModule
local test_store = Work.store

local function add_work(item: WorkItem)
  local ok, err = data.validate(item as any)
  if not ok then
    error("validation failed: " .. (err as string))
  end
  if test_store.items[item.id] then
    error("item id collision: " .. item.id)
  end
  store.add(test_store, item)
end

local function test_validate_string_content_accepts_valid_strings()
  store.reset(test_store)
  local ok, err = data.validate_string_content("this is a valid string", "test")
  assert(ok, "expected ok to be true")
  assert(err == nil, "expected err to be nil")
end
test_validate_string_content_accepts_valid_strings()

local function test_validate_string_content_rejects_double_bracket_close()
  store.reset(test_store)
  local ok, err = data.validate_string_content("string with ]] in it", "test")
  assert(ok == nil, "expected ok to be nil")
  assert(err ~= nil, "expected err to be non-nil")
  assert(err:find("dangerous pattern", 1, true), "expected err to contain 'dangerous pattern'")
  assert(err:find("]]", 1, true), "expected err to contain ']]'")
end
test_validate_string_content_rejects_double_bracket_close()

local function test_validate_string_content_rejects_multiline_comment()
  store.reset(test_store)
  local ok, err = data.validate_string_content("string with --[[ in it", "test")
  assert(ok == nil, "expected ok to be nil")
  assert(err ~= nil, "expected err to be non-nil")
  assert(err:find("dangerous pattern", 1, true), "expected err to contain 'dangerous pattern'")
end
test_validate_string_content_rejects_multiline_comment()

local function test_validate_string_content_rejects_multiline_comment_with_level()
  store.reset(test_store)
  local ok, err = data.validate_string_content("string with --[= in it", "test")
  assert(ok == nil, "expected ok to be nil")
  assert(err ~= nil, "expected err to be non-nil")
  assert(err:find("dangerous pattern", 1, true), "expected err to contain 'dangerous pattern'")
end
test_validate_string_content_rejects_multiline_comment_with_level()

local function test_validate_string_content_rejects_nested_long_string()
  store.reset(test_store)
  local ok, err = data.validate_string_content("string with [=[ in it", "test")
  assert(ok == nil, "expected ok to be nil")
  assert(err ~= nil, "expected err to be non-nil")
  assert(err:find("dangerous pattern", 1, true), "expected err to contain 'dangerous pattern'")
end
test_validate_string_content_rejects_nested_long_string()

local function test_validate_string_content_rejects_null_byte()
  store.reset(test_store)
  local ok, err = data.validate_string_content("string with \0 null byte", "test")
  assert(ok == nil, "expected ok to be nil")
  assert(err ~= nil, "expected err to be non-nil")
  assert(err:find("dangerous pattern", 1, true), "expected err to contain 'dangerous pattern'")
end
test_validate_string_content_rejects_null_byte()

local function test_title_with_dangerous_pattern()
  store.reset(test_store)
  local result = pcall(add_work, {
    id = "01TEST0000000000000000001",
    title = "task with ]] dangerous pattern",
    created = "2025-12-03",
  })

  assert(not result, "expected result to be false")
end
test_title_with_dangerous_pattern()

local function test_description_with_dangerous_pattern()
  store.reset(test_store)
  local result = pcall(add_work, {
    id = "01TEST0000000000000000002",
    title = "safe title",
    description = "description with --[[ comment injection",
    created = "2025-12-03",
  })

  assert(not result, "expected result to be false")
end
test_description_with_dangerous_pattern()

local function test_log_message_with_dangerous_pattern()
  store.reset(test_store)
  local result = pcall(add_work, {
    id = "01TEST0000000000000000003",
    title = "safe title",
    created = "2025-12-03",
    log = {
      ["2025-12-03T10:00:00Z"] = "log message with ]] pattern",
    },
  })

  assert(not result, "expected result to be false")
end
test_log_message_with_dangerous_pattern()

local function test_valid_item_with_all_string_fields()
  store.reset(test_store)
  add_work{
    id = "01TEST0000000000000000004",
    title = "safe title with special chars !@#$%^&*()",
    description = "safe description with unicode: \u{1F600}",
    created = "2025-12-03",
    log = {
      ["2025-12-03T10:00:00Z"] = "normal log message",
      ["2025-12-03T10:01:00Z"] = "another safe message",
    },
  }

  assert(test_store.items["01TEST0000000000000000004"] ~= nil, "expected item to exist")
end
test_valid_item_with_all_string_fields()

local function test_error_message_includes_field_name()
  store.reset(test_store)
  local ok, err = data.validate({
    id = "01TEST0000000000000000005",
    title = "title with ]] bad pattern",
    created = "2025-12-03",
  })

  assert(ok == nil, "expected ok to be nil")
  assert(err ~= nil, "expected err to be non-nil")
  assert(err:find("title", 1, true), "expected err to contain 'title'")
end
test_error_message_includes_field_name()
