local data = require("work.data")

local function Work(item)
  local ok, err = data.validate(item)
  if not ok then
    error("validation failed: " .. err)
  end

  if data.items[item.id] then
    error("item id collision: " .. item.id)
  end

  data.items[item.id] = item
end

return Work
