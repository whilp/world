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

-- Inserts text by simulating keystrokes
function M.insertText(text)
  hs.eventtap.keyStrokes(text)
end

return M
