local M = {}

-- Creates a cached picker from a data module
-- dataModule: array of data items (e.g., emoji or symbols)
-- itemToChoice: function that transforms a data item into a choice object
-- Returns: { getChoices, clearCache }
function M.createCachedPicker(dataModule, itemToChoice)
  local cachedChoices = nil

  local function getChoices()
    if cachedChoices then
      return cachedChoices
    end

    local choices = {}
    for _, item in ipairs(dataModule) do
      table.insert(choices, itemToChoice(item))
    end

    cachedChoices = choices
    return choices
  end

  return {
    getChoices = getChoices,
    clearCache = function()
      cachedChoices = nil
    end
  }
end

-- Inserts text using the pasteboard
-- This properly handles multi-byte UTF-8 characters like emoji
function M.insertText(text)
  local previousContents = hs.pasteboard.getContents()
  hs.pasteboard.setContents(text)
  hs.eventtap.keyStroke({"cmd"}, "v")

  hs.timer.doAfter(0.1, function()
    if previousContents then
      hs.pasteboard.setContents(previousContents)
    end
  end)
end

return M
