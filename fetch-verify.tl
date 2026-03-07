--- fetch a url, verify its sha256, and write to a file.
local fs = require("cosmic.fs")
local cio = require("cosmic.io")
local fetch = require("cosmic.fetch")
local hash = require("cosmic.hash")
local proc = require("cosmic.proc")

--- fetch url, verify sha256, write to path.
--- returns true on success, nil + error on failure.
local function fetch_and_verify(url: string, sha: string, path: string): boolean, string
  local result = fetch.Fetch(url, {
      max_attempts = 3,
      max_delay = 10,
      should_retry = function(r: fetch.Result): boolean
        return r.status >= 500
      end,
    })

  if not result.ok then
    return nil, "fetch " .. url .. ": " .. (result.error or "status " .. tostring(result.status))
  end

  if result.status >= 300 then
    return nil, "fetch " .. url .. ": status " .. tostring(result.status)
  end

  local got = hash.sha256_hex(result.body)
  if got ~= sha then
    return nil, "sha256 mismatch for " .. url .. ": expected " .. sha .. ", got " .. got
  end

  local dir = fs.dirname(path)
  local ok, err = fs.makedirs(dir)
  if not ok then
    return nil, "mkdir " .. dir .. ": " .. (err or "unknown")
  end

  local wok, werr = cio.barf(path, result.body)
  if not wok then
    return nil, "write " .. path .. ": " .. (werr or "unknown")
  end

  return true
end

if proc.is_main() then
  if #arg < 3 then
    io.stderr:write("usage: fetch-verify <url> <sha256> <path>\n")
    os.exit(1)
  end

  local url = arg[1]
  local sha = arg[2]
  local path = arg[3]

  local ok, err = fetch_and_verify(url, sha, path)
  if not ok then
    io.stderr:write("error: " .. (err or "unknown") .. "\n")
    os.exit(1)
  end
end

return {
  fetch_and_verify = fetch_and_verify,
}
