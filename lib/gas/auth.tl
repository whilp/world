-- OAuth2 authentication for Google Apps Script API
-- Uses refresh token flow (user authenticates once via browser)

local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

local TOKEN_URL <const> = "https://oauth2.googleapis.com/token"
local AUTH_URL <const> = "https://accounts.google.com/o/oauth2/v2/auth"

-- scopes needed for Apps Script API
local SCOPES <const> = {
  "https://www.googleapis.com/auth/script.projects",
  "https://www.googleapis.com/auth/script.deployments",
  "https://www.googleapis.com/auth/drive.metadata.readonly",
}

local record Credentials
  client_id: string
  client_secret: string
  refresh_token: string
end

local record TokenResponse
  access_token: string
  expires_in: number
  token_type: string
  scope: string
  refresh_token: string
  error: string
  error_description: string
end

local record AuthState
  credentials: Credentials
  access_token: string
  expires_at: number
end

-- URL encode a string
local function url_encode(s: string): string
  return s:gsub("([^%w%-%.%_%~])", function(c: string): string
    return string.format("%%%02X", c:byte())
  end)
end

-- build URL-encoded form body
local function build_form(params: {string:string}): string
  local parts: {string} = {}
  for k, v in pairs(params) do
    parts[#parts + 1] = url_encode(k) .. "=" .. url_encode(v)
  end
  return table.concat(parts, "&")
end

-- get default credentials path
local function get_credentials_path(): string
  local home = os.getenv("HOME")
  if not home then return nil end
  return path.join(home, ".config", "gas", "credentials.json")
end

-- load credentials from file
local function load_credentials(creds_path: string): Credentials, string
  creds_path = creds_path or get_credentials_path()
  if not creds_path then
    return nil, "could not determine credentials path"
  end

  local content = cosmo.Slurp(creds_path)
  if not content then
    return nil, "credentials file not found: " .. creds_path
  end

  local ok, creds = pcall(cosmo.DecodeJson, content)
  if not ok then
    return nil, "failed to parse credentials: " .. tostring(creds)
  end

  local c = creds as Credentials
  if not c.client_id or not c.client_secret then
    return nil, "credentials missing client_id or client_secret"
  end

  return c
end

-- save credentials to file
local function save_credentials(creds: Credentials, creds_path: string): boolean, string
  creds_path = creds_path or get_credentials_path()
  if not creds_path then
    return nil, "could not determine credentials path"
  end

  local dir = path.dirname(creds_path)
  local ok, err = unix.makedirs(dir)
  if not ok and err ~= "EEXIST" then
    return nil, "failed to create credentials directory: " .. tostring(err)
  end

  local json = cosmo.EncodeJson(creds as {string:any})
  if not cosmo.Barf(creds_path, json, tonumber("600", 8)) then
    return nil, "failed to write credentials file"
  end

  return true
end

-- generate OAuth2 authorization URL
local function get_auth_url(client_id: string, redirect_uri: string): string
  redirect_uri = redirect_uri or "urn:ietf:wg:oauth:2.0:oob"

  local params: {string:string} = {
    client_id = client_id,
    redirect_uri = redirect_uri,
    response_type = "code",
    scope = table.concat(SCOPES, " "),
    access_type = "offline",
    prompt = "consent",
  }

  local query_parts: {string} = {}
  for k, v in pairs(params) do
    query_parts[#query_parts + 1] = url_encode(k) .. "=" .. url_encode(v)
  end

  return AUTH_URL .. "?" .. table.concat(query_parts, "&")
end

-- exchange authorization code for tokens
local function exchange_code(client_id: string, client_secret: string, code: string, redirect_uri: string): TokenResponse, string
  redirect_uri = redirect_uri or "urn:ietf:wg:oauth:2.0:oob"

  local body = build_form({
    client_id = client_id,
    client_secret = client_secret,
    code = code,
    grant_type = "authorization_code",
    redirect_uri = redirect_uri,
  })

  local status, headers, response = cosmo.Fetch(TOKEN_URL, {
    method = "POST",
    headers = {["Content-Type"] = "application/x-www-form-urlencoded"},
    body = body,
  } as {string:any})

  if not status then
    return nil, "token request failed: " .. tostring(headers)
  end

  local ok, result = pcall(cosmo.DecodeJson, response)
  if not ok then
    return nil, "failed to parse token response: " .. tostring(result)
  end

  local token = result as TokenResponse
  if token.error then
    return nil, token.error .. ": " .. (token.error_description or "")
  end

  return token
end

-- refresh access token using refresh token
local function refresh_access_token(creds: Credentials): string, number, string
  if not creds.refresh_token then
    return nil, nil, "no refresh token available"
  end

  local body = build_form({
    client_id = creds.client_id,
    client_secret = creds.client_secret,
    refresh_token = creds.refresh_token,
    grant_type = "refresh_token",
  })

  local status, headers, response = cosmo.Fetch(TOKEN_URL, {
    method = "POST",
    headers = {["Content-Type"] = "application/x-www-form-urlencoded"},
    body = body,
  } as {string:any})

  if not status then
    return nil, nil, "token refresh failed: " .. tostring(headers)
  end

  local ok, result = pcall(cosmo.DecodeJson, response)
  if not ok then
    return nil, nil, "failed to parse token response: " .. tostring(result)
  end

  local token = result as TokenResponse
  if token.error then
    return nil, nil, token.error .. ": " .. (token.error_description or "")
  end

  local expires_at = os.time() + (token.expires_in or 3600) - 60
  return token.access_token, expires_at
end

-- create auth state from credentials
local function create_auth(creds_path: string): AuthState, string
  local creds, err = load_credentials(creds_path)
  if not creds then
    return nil, err
  end

  return {
    credentials = creds,
    access_token = nil,
    expires_at = 0,
  }
end

-- get valid access token, refreshing if needed
local function get_access_token(auth: AuthState): string, string
  if auth.access_token and os.time() < auth.expires_at then
    return auth.access_token
  end

  local token, expires_at, err = refresh_access_token(auth.credentials)
  if not token then
    return nil, err
  end

  auth.access_token = token
  auth.expires_at = expires_at
  return token
end

return {
  SCOPES = SCOPES,
  load_credentials = load_credentials,
  save_credentials = save_credentials,
  get_auth_url = get_auth_url,
  exchange_code = exchange_code,
  create_auth = create_auth,
  get_access_token = get_access_token,
}
