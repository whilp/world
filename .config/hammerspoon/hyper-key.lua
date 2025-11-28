local HyperKey = {}
HyperKey.__index = HyperKey

function HyperKey.new(mods)
  local self = setmetatable({}, HyperKey)
  self.mods = mods
  self.bindings = {}
  return self
end

function HyperKey:bind(key)
  return {
    toFunction = function(_, name, fn)
      hs.hotkey.bind(self.mods, key, fn)
      table.insert(self.bindings, {key = key, name = name})
      return self
    end,
    toApplication = function(_, app)
      hs.hotkey.bind(self.mods, key, function()
        hs.application.launchOrFocus(app)
      end)
      table.insert(self.bindings, {key = key, name = "Launch " .. app})
      return self
    end
  }
end

function HyperKey:getBindings()
  return self.bindings
end

return HyperKey
