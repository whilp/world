local cosmo = require("cosmo")

local ENCODING <const> = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
local ENCODING_LEN <const> = 32
local TIME_LEN <const> = 10
local RANDOM_LEN <const> = 16
local TOTAL_LEN <const> = TIME_LEN + RANDOM_LEN

local record DecodedUlid
  timestamp: number
  time: string
  random: string
end

local function encode_time(timestamp: number, len: integer): string
  local result: {string} = {}
  for i = len, 1, -1 do
    local mod = math.floor(timestamp % ENCODING_LEN) as integer
    result[i] = ENCODING:sub(mod + 1, mod + 1)
    timestamp = math.floor(timestamp / ENCODING_LEN)
  end
  return table.concat(result)
end

local function encode_random(len: integer): string
  local result: {string} = {}
  local bytes = cosmo.GetRandomBytes(len)
  for i = 1, len do
    local byte = bytes:byte(i)
    result[i] = ENCODING:sub((byte % ENCODING_LEN) + 1, (byte % ENCODING_LEN) + 1)
  end
  return table.concat(result)
end

local function generate(): string
  local timestamp = math.floor(os.time() * 1000)
  local time_str = encode_time(timestamp, TIME_LEN)
  local random_str = encode_random(RANDOM_LEN)
  return time_str .. random_str
end

local function timestamp(ulid: string): number, string
  if type(ulid) ~= "string" or #ulid ~= TOTAL_LEN then
    return nil, "invalid ulid"
  end

  local ts = 0
  for i = 1, TIME_LEN do
    local char = ulid:sub(i, i)
    local pos = ENCODING:find(char, 1, true)
    if not pos then
      return nil, "invalid character in ulid"
    end
    ts = ts * ENCODING_LEN + (pos - 1)
  end

  return ts
end

local function decode(ulid: string): DecodedUlid, string
  local ts, err = timestamp(ulid)
  if not ts then
    return nil, err
  end

  return {
    timestamp = ts,
    time = os.date("!%Y-%m-%dT%H:%M:%S", math.floor(ts / 1000)),
    random = ulid:sub(TIME_LEN + 1)
  }
end

return {
  generate = generate,
  timestamp = timestamp,
  decode = decode,
}
