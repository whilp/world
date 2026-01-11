--test:false

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
  validate: function(item: any): boolean, string
end

local record StoreModule
  new: function(): WorkStore
  add: function(store: WorkStore, item: WorkItem): WorkStore
end

local data = require("work.data") as DataModule
local store = require("work.store") as StoreModule

local _test_store: WorkStore = store.new()

local function Work(item: WorkItem)
  local ok, err = data.validate(item as any)
  if not ok then
    error("validation failed: " .. err)
  end

  if _test_store.items[item.id] then
    error("item id collision: " .. item.id)
  end

  store.add(_test_store, item)
end

local record WorkModule
  store: WorkStore
end

local module: WorkModule = { store = _test_store }

return setmetatable(module as {string:any}, {
  __call = function(_: any, item: WorkItem)
    return Work(item)
  end
})
