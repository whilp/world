local M = {}

M.url = "https://www.unicode.org/Public/17.0.0/emoji/emoji-test.txt"

local function read_file(path)
  local f = io.open(path, "r")
  if not f then
    return nil, "failed to open file: " .. path
  end
  local content = f:read("*all")
  f:close()
  return content
end

M.parse_emoji_test = function(content)
  local emoji_list = {}
  local current_group = nil
  local current_subgroup = nil

  for line in content:gmatch("[^\r\n]+") do
    local group_match = line:match("^# group: (.+)$")
    if group_match then
      current_group = group_match
      current_subgroup = nil
    else
      local subgroup_match = line:match("^# subgroup: (.+)$")
      if subgroup_match then
        current_subgroup = subgroup_match
      elseif not line:match("^%s*#") and not line:match("^%s*$") then
        local codepoints, status, rest = line:match("^([^;]+);%s*([^#]+)#%s*(.+)$")
        if codepoints and status and rest then
          codepoints = codepoints:match("^%s*(.-)%s*$")
          status = status:match("^%s*(.-)%s*$")

          local emoji_char, version, name = rest:match("^(.-)%s+E(%d+%.%d+)%s+(.+)$")
          if emoji_char and name then
            emoji_char = emoji_char:match("^%s*(.-)%s*$")
            name = name:match("^%s*(.-)%s*$")

            if status == "fully-qualified" then
              table.insert(emoji_list, {
                codepoints = codepoints,
                emoji = emoji_char,
                name = name,
                version = version,
                status = status,
                group = current_group,
                subgroup = current_subgroup
              })
            end
          end
        end
      end
    end
  end

  return emoji_list
end

M.load_from_file = function(path)
  local content, err = read_file(path)
  if not content then
    return nil, err
  end
  return M.parse_emoji_test(content)
end

return M
