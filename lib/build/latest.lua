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
  return nil
end

function M.check(version_file, output_file, opts)
  opts = opts or {}
  local stderr = opts.stderr or io.stderr

  local tool_name = M.infer_tool_name(version_file)
  local strategy = tool_name and strategies[tool_name]

  local latest
  if strategy then
    stderr:write("checking " .. tool_name .. " version...\n")
    local fetch_err
    latest, fetch_err = strategy.fetch_latest(opts)
    if not latest then
      stderr:write("warning: " .. fetch_err .. "\n")
      stderr:write("falling back to current version\n")
      local ok, current = pcall(dofile, version_file)
      if not ok then
        return nil, "failed to load " .. version_file .. ": " .. tostring(current)
      end
      latest = current
      latest._todo = true
    end
  else
    stderr:write("loading current version from " .. version_file .. "...\n")
    local ok, current = pcall(dofile, version_file)
    if not ok then
      return nil, "failed to load " .. version_file .. ": " .. tostring(current)
    end
    latest = current
    latest._todo = true
  end

  local content = "return " .. cosmo.EncodeLua(latest, {pretty = true})

  local output_dir = path.dirname(output_file)
  unix.makedirs(output_dir)

  if not cosmo.Barf(output_file, content) then
    return nil, "failed to write " .. output_file
  end

  stderr:write("wrote " .. output_file .. "\n")
  return true
end

function M.report(output_dir)
  local walk = require("walk")

  local up_to_date = {}
  local todo = {}
  local errors = {}

  for _, filepath in ipairs(walk.collect(output_dir, "%.latest%.ok$")) do
    local chunk, load_err = loadfile(filepath)
    if chunk then
      local ok, result = pcall(chunk)
      if ok and result then
        if result._todo then
          table.insert(todo, filepath)
        else
          table.insert(up_to_date, filepath)
        end
      else
        table.insert(errors, {file = filepath, error = tostring(result)})
      end
    else
      table.insert(errors, {file = filepath, error = tostring(load_err)})
    end
  end

  local total = #up_to_date + #todo + #errors

  print(string.format("Latest version check summary:"))
  print(string.format("  %d total version files", total))
  print(string.format("  %d up-to-date (with strategies)", #up_to_date))
  print(string.format("  %d need work (_todo flag)", #todo))

  if #errors > 0 then
    print(string.format("  %d errors", #errors))
    for _, err_info in ipairs(errors) do
      print(string.format("    %s: %s", err_info.file, err_info.error))
    end
  end

  if #todo > 0 then
    print("\nFiles needing strategies:")
    table.sort(todo)
    for _, file in ipairs(todo) do
      local short_name = file:gsub("^o/any/", ""):gsub("%.latest%.ok$", "")
      print(string.format("  - %s", short_name))
    end
  end

  return #errors == 0
end

if not pcall(debug.getlocal, 4, 1) then
  local command = arg[1]

  if command == "report" then
    local output_dir = arg[2]
    if not output_dir then
      io.stderr:write("usage: latest.lua report <output_dir>\n")
      os.exit(1)
    end

    local ok = M.report(output_dir)
    os.exit(ok and 0 or 1)
  else
    local version_file = arg[1]
    local output_file = arg[2]
    if not version_file or not output_file then
      io.stderr:write("usage: latest.lua <version_file> <output_file>\n")
      io.stderr:write("       latest.lua report <output_dir>\n")
      os.exit(1)
    end

    local ok, err = M.check(version_file, output_file)
    if not ok then
      io.stderr:write("error: " .. err .. "\n")
      os.exit(1)
    end
  end
end

return M
