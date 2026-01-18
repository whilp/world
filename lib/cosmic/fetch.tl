-- cosmic.fetch: structured wrapper for cosmo.Fetch
--
-- Solves the problem of accidentally discarding error messages when using:
--   local status, _, body = cosmo.Fetch(url, opts)
--
-- Usage:
--   local fetch = require("cosmic.fetch")
--   local result = fetch.Fetch(url, opts)
--   if not result.ok then
--     return nil, result.error
--   end
--   -- use result.status, result.headers, result.body

local cosmo = require("cosmo")

local record FetchOpts
  method: string
  headers: {string:string}
  body: string
  maxresponse: number
end

local record FetchResult
  ok: boolean
  status: number
  headers: {string:string}
  body: string
  error: string
end

-- Fetch wraps cosmo.Fetch to return a structured result table.
-- On success: ok=true, status=200, headers={...}, body="..."
-- On failure: ok=false, error="error message"
local function Fetch(url: string, opts?: FetchOpts): FetchResult
  local status: number
  local headers_or_err: {string:string} | string
  local body: string

  status, headers_or_err, body = cosmo.Fetch(url, opts as {string:any})

  if not status then
    -- Fetch failed: headers_or_err contains error message
    return {
      ok = false,
      error = tostring(headers_or_err or "unknown error"),
    }
  end

  -- Fetch succeeded: headers_or_err is the headers table
  return {
    ok = true,
    status = status,
    headers = headers_or_err as {string:string},
    body = body,
    error = nil,
  }
end

local record fetch
  Fetch: function(url: string, opts?: FetchOpts): FetchResult
  FetchOpts: FetchOpts
  FetchResult: FetchResult
end

local M: fetch = {
  Fetch = Fetch,
}

return M
