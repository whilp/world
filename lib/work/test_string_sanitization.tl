local lu = require("luaunit")

-- skip if posix not available (work module requires luaposix)
local has_posix = pcall(require, "posix")
if not has_posix then
  global function test_string_sanitization_skipped()
    lu.skip("requires luaposix")
  end
  return
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

global TestStringSanitization: {string:any} = {}

function TestStringSanitization:setUp()
  store.reset(test_store)
end

function TestStringSanitization:test_validate_string_content_accepts_valid_strings()
  local ok, err = data.validate_string_content("this is a valid string", "test")
  lu.assertTrue(ok)
  lu.assertNil(err)
end

function TestStringSanitization:test_validate_string_content_rejects_double_bracket_close()
  local ok, err = data.validate_string_content("string with ]] in it", "test")
  lu.assertNil(ok)
  lu.assertNotNil(err)
  lu.assertStrContains(err, "dangerous pattern")
  lu.assertStrContains(err, "]]")
end

function TestStringSanitization:test_validate_string_content_rejects_multiline_comment()
  local ok, err = data.validate_string_content("string with --[[ in it", "test")
  lu.assertNil(ok)
  lu.assertNotNil(err)
  lu.assertStrContains(err, "dangerous pattern")
end

function TestStringSanitization:test_validate_string_content_rejects_multiline_comment_with_level()
  local ok, err = data.validate_string_content("string with --[= in it", "test")
  lu.assertNil(ok)
  lu.assertNotNil(err)
  lu.assertStrContains(err, "dangerous pattern")
end

function TestStringSanitization:test_validate_string_content_rejects_nested_long_string()
  local ok, err = data.validate_string_content("string with [=[ in it", "test")
  lu.assertNil(ok)
  lu.assertNotNil(err)
  lu.assertStrContains(err, "dangerous pattern")
end

function TestStringSanitization:test_validate_string_content_rejects_null_byte()
  local ok, err = data.validate_string_content("string with \0 null byte", "test")
  lu.assertNil(ok)
  lu.assertNotNil(err)
  lu.assertStrContains(err, "dangerous pattern")
end

function TestStringSanitization:test_title_with_dangerous_pattern()
  local result = pcall(add_work, {
    id = "01TEST0000000000000000001",
    title = "task with ]] dangerous pattern",
    created = "2025-12-03",
  })

  lu.assertFalse(result)
end

function TestStringSanitization:test_description_with_dangerous_pattern()
  local result = pcall(add_work, {
    id = "01TEST0000000000000000002",
    title = "safe title",
    description = "description with --[[ comment injection",
    created = "2025-12-03",
  })

  lu.assertFalse(result)
end

function TestStringSanitization:test_log_message_with_dangerous_pattern()
  local result = pcall(add_work, {
    id = "01TEST0000000000000000003",
    title = "safe title",
    created = "2025-12-03",
    log = {
      ["2025-12-03T10:00:00Z"] = "log message with ]] pattern",
    },
  })

  lu.assertFalse(result)
end

function TestStringSanitization:test_valid_item_with_all_string_fields()
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

  lu.assertNotNil(test_store.items["01TEST0000000000000000004"])
end

function TestStringSanitization:test_error_message_includes_field_name()
  local ok, err = data.validate({
    id = "01TEST0000000000000000005",
    title = "title with ]] bad pattern",
    created = "2025-12-03",
  })

  lu.assertNil(ok)
  lu.assertNotNil(err)
  lu.assertStrContains(err, "title")
end
