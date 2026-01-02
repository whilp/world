#!/usr/bin/env lua
local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

local M = {}

local function fetch_json(url)
  local status, _, body = cosmo.Fetch(url, {
    headers = {["User-Agent"] = "curl/8.0", ["Accept"] = "application/vnd.github+json"},
  })
  if status ~= 200 then
    return nil, "fetch failed: " .. tostring(status)
  end
  return cosmo.DecodeJson(body)
end

local function fetch_sha256(url)
  local status, headers, body = cosmo.Fetch(url, {
    headers = {["User-Agent"] = "curl/8.0"},
    maxresponse = 300 * 1024 * 1024,
  })
  if not status then
    return nil, "fetch failed: " .. tostring(headers or "unknown error")
  end
  if status ~= 200 then
    return nil, "fetch failed with status: " .. tostring(status)
  end
  return cosmo.EncodeHex(cosmo.Sha256(body)):lower()
end

local function fetch_text(url)
  local status, _, body = cosmo.Fetch(url, {
    headers = {["User-Agent"] = "curl/8.0"},
  })
  if status ~= 200 then
    return nil, "fetch failed: " .. tostring(status)
  end
  return body
end

local nvim_strategy = {}

function nvim_strategy.fetch_latest(opts)
  opts = opts or {}
  local stderr = opts.stderr or io.stderr
  local repo = "whilp/neovim"
  local api_url = "https://api.github.com/repos/" .. repo .. "/releases/latest"

  stderr:write("fetching release info...\n")
  local release, err = fetch_json(api_url)
  if not release then
    return nil, err
  end

  local tag = release.tag_name
  local date, short_sha = tag:match("^(%d+%.%d+%.%d+)%-([0-9a-f]+)$")
  if not date or not short_sha then
    return nil, "could not parse tag: " .. tag
  end

  local platform_map = {
    ["nvim-macos-arm64.tar.gz"] = "darwin-arm64",
    ["nvim-linux-arm64.tar.gz"] = "linux-arm64",
    ["nvim-linux-x86_64.tar.gz"] = "linux-x86_64",
  }

  local assets = {}
  for _, asset in ipairs(release.assets) do
    local platform = platform_map[asset.name]
    if platform then
      assets[platform] = asset.browser_download_url
    end
  end

  stderr:write("fetching commit info...\n")
  local commit_url = "https://api.github.com/repos/" .. repo .. "/commits/" .. short_sha
  local commit, commit_err = fetch_json(commit_url)
  if not commit then
    return nil, commit_err
  end
  local full_sha = commit.sha
  local version = "0.12.0-dev-" .. full_sha:sub(1, 10)

  local platforms = {}
  for platform, url in pairs(assets) do
    stderr:write("fetching sha256 for " .. platform .. "...\n")
    local sha, fetch_err = fetch_sha256(url)
    if not sha then
      return nil, fetch_err
    end
    local plat = {sha = sha}
    if platform == "darwin-arm64" then
      plat.platform = "macos-arm64"
    else
      plat.platform = platform
    end
    platforms[platform] = plat
  end

  return {
    version = version,
    date = date,
    tag = tag,
    format = "tar.gz",
    strip_components = 1,
    url = "https://github.com/" .. repo .. "/releases/download/{tag}/nvim-{platform}.tar.gz",
    platforms = platforms,
  }
end

local cosmos_strategy = {}

function cosmos_strategy.fetch_latest(opts)
  opts = opts or {}
  local stderr = opts.stderr or io.stderr
  local repo = "whilp/cosmopolitan"

  local api_url = "https://api.github.com/repos/" .. repo .. "/releases/latest"
  local release, err = fetch_json(api_url)
  if not release then
    return nil, err
  end

  local version = release.tag_name

  stderr:write("fetching sha256sums for " .. version .. "...\n")
  local sums_url = string.format("https://github.com/%s/releases/download/%s/SHA256SUMS", repo, version)
  local body, sums_err = fetch_text(sums_url)
  if not body then
    return nil, sums_err
  end

  local sums = {}
  for line in body:gmatch("[^\n]+") do
    local sha, name = line:match("^(%x+)%s+(.+)$")
    if sha and name then
      sums[name] = sha:lower()
    end
  end

  local sha = sums["cosmos.zip"]
  if not sha then
    return nil, "no sha256 for cosmos.zip"
  end

  return {
    version = version,
    format = "zip",
    url = "https://github.com/" .. repo .. "/releases/download/{version}/cosmos.zip",
    platforms = {["*"] = {sha = sha}},
  }
end

local claude_strategy = {}

function claude_strategy.fetch_latest(opts)
  opts = opts or {}
  local stderr = opts.stderr or io.stderr
  local base_url = "https://storage.googleapis.com/claude-code-dist-86c565f3-f756-42ad-8dfa-d59b1c096819"

  local api_url = "https://api.github.com/repos/anthropics/claude-code/releases/latest"
  local release, err = fetch_json(api_url)
  if not release then
    return nil, err
  end

  local version = release.tag_name:match("^v?(.+)$")

  local url = string.format("%s/claude-code-releases/%s/linux-x64/claude", base_url, version)

  stderr:write("fetching sha256 for linux-x64...\n")
  local sha256, sha_err = fetch_sha256(url)
  if not sha256 then
    return nil, sha_err
  end

  return {
    version = version,
    base_url = base_url,
    url = "{base_url}/claude-code-releases/{version}/{platform}/claude",
    platforms = {
      ["linux-x64"] = {sha = sha256},
    },
  }
end

local strategies = {
  nvim = nvim_strategy,
  cosmos = cosmos_strategy,
  claude = claude_strategy,
}

function M.infer_tool_name(version_file)
  if version_file:match("nvim") then
    return "nvim"
  elseif version_file:match("cosmos") then
    return "cosmos"
  elseif version_file:match("claude") then
    return "claude"
  end
  return nil, "could not infer tool name from: " .. version_file
end

function M.check(version_file, opts)
  opts = opts or {}
  local stderr = opts.stderr or io.stderr

  local tool_name, err = M.infer_tool_name(version_file)
  if not tool_name then
    return nil, err
  end

  local strategy = strategies[tool_name]
  if not strategy then
    return nil, "no strategy for tool: " .. tool_name
  end

  stderr:write("checking " .. tool_name .. " version...\n")
  local latest, fetch_err = strategy.fetch_latest(opts)
  if not latest then
    return nil, fetch_err
  end

  local ok_file = version_file:gsub("%.lua$", ".latest.ok")
  local content = "return " .. cosmo.EncodeLua(latest, {pretty = true})

  if not cosmo.Barf(ok_file, content) then
    return nil, "failed to write " .. ok_file
  end

  stderr:write("wrote " .. ok_file .. "\n")
  return true
end

if not pcall(debug.getlocal, 4, 1) then
  local version_file = arg[1]
  if not version_file then
    io.stderr:write("usage: latest.lua <version_file>\n")
    os.exit(1)
  end

  local ok, err = M.check(version_file)
  if not ok then
    io.stderr:write("error: " .. err .. "\n")
    os.exit(1)
  end
end

return M
