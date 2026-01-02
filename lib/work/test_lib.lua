local lu = require("luaunit")

-- skip if posix not available (work module requires luaposix)
local has_posix = pcall(require, "posix")
if not has_posix then
  function test_work_lib_skipped()
    lu.skip("requires luaposix")
  end
  return setmetatable({}, {
    __call = function() end
  })
end

local data = require("work.data")
local store = require("work.store")

local _test_store = store.new()

local function Work(item)
  local ok, err = data.validate(item)
  if not ok then
    error("validation failed: " .. err)
  end

  if _test_store.items[item.id] then
    error("item id collision: " .. item.id)
  end

  store.add(_test_store, item)
end

return setmetatable({ store = _test_store }, {
  __call = function(_, item)
    return Work(item)
  end
})
