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
--
-- With retry:
--   local result = fetch.Fetch(url, {max_attempts = 4})
--   local result = fetch.Fetch(url, {should_retry = function(r) return r.status >= 500 end})

local cosmo = require("cosmo")
local unix = require("cosmo.unix")

local record Result
  ok: boolean
  status: number
  headers: {string:string}
  body: string
  error: string
end

local record Opts
  method: string
  headers: {string:string}
  body: string
  maxresponse: number
  max_attempts: number
  max_delay: number
  should_retry: function(Result): boolean
end

local function do_fetch(url: string, opts?: Opts): Result
  local status: number
  local headers_or_err: {string:string} | string
  local body: string

  status, headers_or_err, body = cosmo.Fetch(url, opts as {string:any})

  if not status then
    return {ok = false, error = tostring(headers_or_err or "unknown error")}
  end

  return {
    ok = true,
    status = status,
    headers = headers_or_err as {string:string},
    body = body,
  }
end

-- Fetch wraps cosmo.Fetch with structured results and optional retry.
-- Set max_attempts > 1 for retry with exponential backoff.
-- Set should_retry to control which results trigger retry.
local function Fetch(url: string, opts?: Opts): Result
  local max_attempts = (opts and opts.max_attempts) or 1
  local max_delay = (opts and opts.max_delay) or 30
  local should_retry = opts and opts.should_retry

  local result: Result
  for attempt = 1, max_attempts do
    result = do_fetch(url, opts)

    if not result.ok or not should_retry or not should_retry(result) then
      return result
    end

    if attempt < max_attempts then
      unix.nanosleep(math.min(max_delay, 2 ^ attempt), 0)
    end
  end

  return result
end

local record fetch
  Fetch: function(url: string, opts?: Opts): Result
  Opts: Opts
  Result: Result
end

local M: fetch = {
  Fetch = Fetch,
}

return M
