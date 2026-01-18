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
--   local result = fetch.FetchWithRetry(url, opts, {max_attempts = 4})

local cosmo = require("cosmo")
local unix = require("cosmo.unix")

local record Opts
  method: string
  headers: {string:string}
  body: string
  maxresponse: number
end

local record Result
  ok: boolean
  status: number
  headers: {string:string}
  body: string
  error: string
end

local record RetryOpts
  max_attempts: number
  max_delay: number
  retry_on_status: function(number): boolean
end

-- Fetch wraps cosmo.Fetch to return a structured result table.
-- On success: ok=true, status=200, headers={...}, body="..."
-- On failure: ok=false, error="error message"
local function Fetch(url: string, opts?: Opts): Result
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

-- FetchWithRetry wraps Fetch with exponential backoff retry logic.
-- Retries on network errors and optionally on specific HTTP status codes.
-- Default: 4 attempts, 30s max delay, retry on 5xx status codes.
local function FetchWithRetry(url: string, opts: Opts, retry_opts?: RetryOpts): Result
  local max_attempts = (retry_opts and retry_opts.max_attempts) or 4
  local max_delay = (retry_opts and retry_opts.max_delay) or 30
  local retry_on_status = retry_opts and retry_opts.retry_on_status

  local result: Result
  for attempt = 1, max_attempts do
    result = Fetch(url, opts)

    if result.ok then
      if not retry_on_status or not retry_on_status(result.status) then
        return result
      end
      result = {ok = false, error = "HTTP " .. tostring(result.status)}
    end

    if attempt < max_attempts then
      local delay = math.min(max_delay, 2 ^ attempt)
      unix.nanosleep(delay, 0)
    end
  end

  return result
end

local record fetch
  Fetch: function(url: string, opts?: Opts): Result
  FetchWithRetry: function(url: string, opts: Opts, retry_opts?: RetryOpts): Result
  Opts: Opts
  Result: Result
  RetryOpts: RetryOpts
end

local M: fetch = {
  Fetch = Fetch,
  FetchWithRetry = FetchWithRetry,
}

return M
