local bindings = {
  Bind("r", "Start recording", { url = "superwhisper://record" })
}

local modes_dir = os.getenv("HOME") .. "/Documents/superwhisper/modes"
local iter, dir_obj = hs.fs.dir(modes_dir)

if iter then
  for file in iter, dir_obj do
    if file:match("%.json$") and file ~= "." and file ~= ".." then
      local path = modes_dir .. "/" .. file
      local f = io.open(path, "r")
      if f then
        local content = f:read("*all")
        f:close()

        local ok, mode = pcall(hs.json.decode, content)
        if ok and mode.key and mode.name then
          local key_char = mode.key:sub(1, 1)
          table.insert(bindings, Bind(key_char, "Switch to " .. mode.name, { url = "superwhisper://mode?key=" .. mode.key }))
        end
      end
    end
  end
end

return Leader("w", "Superwhisper", bindings)
