local M = {}

local ENCODING = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

local ENCODING_LEN = 32
local TIME_LEN = 10
local RANDOM_LEN = 16
local TOTAL_LEN = TIME_LEN + RANDOM_LEN

local function encode_time(timestamp, len)
  local result = {}
  for i = len, 1, -1 do
    local mod = timestamp % ENCODING_LEN
    result[i] = ENCODING:sub(mod + 1, mod + 1)
    timestamp = math.floor(timestamp / ENCODING_LEN)
  end
  return table.concat(result)
end

local function encode_random(len)
  local result = {}
  local f = io.open("/dev/urandom", "rb")
  if not f then
    math.randomseed(os.time() + os.clock() * 1000000)
    for i = 1, len do
      result[i] = ENCODING:sub(math.random(1, ENCODING_LEN), math.random(1, ENCODING_LEN))
    end
  else
    local bytes = f:read(len)
    f:close()
    for i = 1, len do
      local byte = bytes:byte(i)
      result[i] = ENCODING:sub((byte % ENCODING_LEN) + 1, (byte % ENCODING_LEN) + 1)
    end
  end
  return table.concat(result)
end

M.generate = function()
  local timestamp = math.floor(os.time() * 1000)

  local time_str = encode_time(timestamp, TIME_LEN)
  local random_str = encode_random(RANDOM_LEN)

  return time_str .. random_str
end

M.timestamp = function(ulid)
  if type(ulid) ~= "string" or #ulid ~= TOTAL_LEN then
    return nil, "invalid ulid"
  end

  local timestamp = 0
  for i = 1, TIME_LEN do
    local char = ulid:sub(i, i)
    local pos = ENCODING:find(char, 1, true)
    if not pos then
      return nil, "invalid character in ulid"
    end
    timestamp = timestamp * ENCODING_LEN + (pos - 1)
  end

  return timestamp
end

M.decode = function(ulid)
  local ts, err = M.timestamp(ulid)
  if not ts then
    return nil, err
  end

  return {
    timestamp = ts,
    time = os.date("!%Y-%m-%dT%H:%M:%S", math.floor(ts / 1000)),
    random = ulid:sub(TIME_LEN + 1)
  }
end

return M
