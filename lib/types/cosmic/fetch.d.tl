-- type declarations for cosmic.fetch

local record fetch
  record Result
    status: number
    headers: {string:string}
    ok: boolean
    body: string
    error: string
  end

  record FetchOpts
    method: string
    headers: {string:string}
    body: string
    maxresponse: integer
    max_attempts: integer
  end

  Fetch: function(url: string, opts?: FetchOpts): Result
end

return fetch
