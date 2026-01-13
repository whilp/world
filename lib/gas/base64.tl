-- base64 encoding/decoding for OAuth2

local ALPHABET <const> = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
local URL_ALPHABET <const> = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

local function make_decode_table(alphabet: string): {number:number}
  local t: {number:number} = {}
  for i = 1, 64 do
    t[alphabet:byte(i)] = i - 1
  end
  return t
end

local DECODE_TABLE = make_decode_table(ALPHABET)
local URL_DECODE_TABLE = make_decode_table(URL_ALPHABET)

local function encode(data: string, alphabet: string, pad: boolean): string
  alphabet = alphabet or ALPHABET
  if pad == nil then pad = true end

  local result: {string} = {}
  local n = #data
  local i = 1

  while i <= n do
    local b1 = data:byte(i) or 0
    local b2 = data:byte(i + 1) or 0
    local b3 = data:byte(i + 2) or 0

    local c1 = (b1 >> 2) + 1
    local c2 = (((b1 & 3) << 4) | (b2 >> 4)) + 1
    local c3 = (((b2 & 15) << 2) | (b3 >> 6)) + 1
    local c4 = (b3 & 63) + 1

    result[#result + 1] = alphabet:sub(c1, c1)
    result[#result + 1] = alphabet:sub(c2, c2)

    if i + 1 <= n then
      result[#result + 1] = alphabet:sub(c3, c3)
    elseif pad then
      result[#result + 1] = "="
    end

    if i + 2 <= n then
      result[#result + 1] = alphabet:sub(c4, c4)
    elseif pad then
      result[#result + 1] = "="
    end

    i = i + 3
  end

  return table.concat(result)
end

local function decode(data: string, decode_table: {number:number}): string
  decode_table = decode_table or DECODE_TABLE

  -- remove padding
  data = data:gsub("=+$", "")

  local result: {string} = {}
  local n = #data
  local i = 1

  while i <= n do
    local c1 = decode_table[data:byte(i)] or 0
    local c2 = decode_table[data:byte(i + 1)] or 0
    local c3 = decode_table[data:byte(i + 2)]
    local c4 = decode_table[data:byte(i + 3)]

    result[#result + 1] = string.char((c1 << 2) | (c2 >> 4))

    if c3 then
      result[#result + 1] = string.char(((c2 & 15) << 4) | (c3 >> 2))
    end

    if c4 then
      result[#result + 1] = string.char(((c3 & 3) << 6) | c4)
    end

    i = i + 4
  end

  return table.concat(result)
end

-- standard base64 encoding
local function encode_std(data: string): string
  return encode(data, ALPHABET, true)
end

-- URL-safe base64 encoding without padding (for OAuth2/JWT)
local function encode_url(data: string): string
  return encode(data, URL_ALPHABET, false)
end

-- standard base64 decoding
local function decode_std(data: string): string
  return decode(data, DECODE_TABLE)
end

-- URL-safe base64 decoding
local function decode_url(data: string): string
  return decode(data, URL_DECODE_TABLE)
end

return {
  encode = encode_std,
  decode = decode_std,
  encode_url = encode_url,
  decode_url = decode_url,
}
