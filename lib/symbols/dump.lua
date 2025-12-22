local M = {}

M.unicode_data_url = "https://www.unicode.org/Public/17.0.0/ucd/UnicodeData.txt"
M.blocks_url = "https://www.unicode.org/Public/17.0.0/ucd/Blocks.txt"

local function read_file(path)
  local f = io.open(path, "r")
  if not f then
    return nil, "failed to open file: " .. path
  end
  local content = f:read("*all")
  f:close()
  return content
end

local function parse_blocks(content)
  local blocks = {}

  for line in content:gmatch("[^\r\n]+") do
    if not line:match("^%s*#") and not line:match("^%s*$") then
      local range_start, range_end, block_name = line:match("^([0-9A-F]+)%.%.([0-9A-F]+);%s*(.+)%s*$")
      if range_start and range_end and block_name then
        table.insert(blocks, {
          start = tonumber(range_start, 16),
          ["end"] = tonumber(range_end, 16),
          name = block_name
        })
      end
    end
  end

  return blocks
end

local function get_block_name(codepoint_num, blocks)
  for _, block in ipairs(blocks) do
    if codepoint_num >= block.start and codepoint_num <= block["end"] then
      return block.name
    end
  end
  return nil
end

local function codepoint_to_utf8(codepoint_num)
  if codepoint_num < 0x80 then
    return string.char(codepoint_num)
  elseif codepoint_num < 0x800 then
    return string.char(
      0xC0 + math.floor(codepoint_num / 0x40),
      0x80 + (codepoint_num % 0x40)
    )
  elseif codepoint_num < 0x10000 then
    return string.char(
      0xE0 + math.floor(codepoint_num / 0x1000),
      0x80 + (math.floor(codepoint_num / 0x40) % 0x40),
      0x80 + (codepoint_num % 0x40)
    )
  else
    return string.char(
      0xF0 + math.floor(codepoint_num / 0x40000),
      0x80 + (math.floor(codepoint_num / 0x1000) % 0x40),
      0x80 + (math.floor(codepoint_num / 0x40) % 0x40),
      0x80 + (codepoint_num % 0x40)
    )
  end
end

M.parse_unicode_data = function(unicode_content, blocks_content)
  local blocks = parse_blocks(blocks_content)
  local symbols = {}

  for line in unicode_content:gmatch("[^\r\n]+") do
    local fields = {}
    for field in (line .. ";"):gmatch("([^;]*);") do
      table.insert(fields, field)
    end

    if #fields >= 3 then
      local codepoint = fields[1]
      local name = fields[2]
      local category = fields[3]

      if category == "Sm" or category == "So" or category == "Sc" or category == "Sk" then
        local codepoint_num = tonumber(codepoint, 16)
        local symbol = codepoint_to_utf8(codepoint_num)
        local block = get_block_name(codepoint_num, blocks)

        table.insert(symbols, {
          codepoint = codepoint,
          symbol = symbol,
          name = name,
          category = category,
          block = block
        })
      end
    end
  end

  return symbols
end

M.load_from_files = function(unicode_data_path, blocks_path)
  local unicode_content, err = read_file(unicode_data_path)
  if not unicode_content then
    return nil, err
  end

  local blocks_content, err = read_file(blocks_path)
  if not blocks_content then
    return nil, err
  end

  return M.parse_unicode_data(unicode_content, blocks_content)
end

return M
