#!/usr/bin/env lua
local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

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

local function interpolate(template, vars)
  return template:gsub("{([%w_]+)}", function(key)
    return tostring(vars[key] or "{" .. key .. "}")
  end)
end

local function extract_github_repo(url)
  local owner, repo = url:match("github%.com/([^/]+)/([^/]+)")
  if owner and repo then
    return owner .. "/" .. repo
  end
  return nil
end

local function parse_sha256sums(text)
  local sums = {}
  for line in text:gmatch("[^\n]+") do
    local sha, name = line:match("^(%x+)%s+(.+)$")
    if sha and name then
      sums[name] = sha:lower()
    end
  end
  return sums
end

local function try_fetch_sha256sums(repo, version_or_tag)
  local possible_names = {"SHA256SUMS", "sha256sums.txt", "checksums.txt", "SHASUMS256.txt"}

  for _, name in ipairs(possible_names) do
    local url = string.format("https://github.com/%s/releases/download/%s/%s",
      repo, version_or_tag, name)
    local body = fetch_text(url)
    if body then
      return parse_sha256sums(body)
    end
  end

  return nil
end

local function infer_asset_name(url_template, platform_config, platform_key, version_info)
  local vars = {}

  for k, v in pairs(version_info) do
    if type(v) ~= "table" then
      vars[k] = v
    end
  end

  for k, v in pairs(platform_config) do
    if type(v) ~= "table" then
      vars[k] = v
    end
  end

  vars.platform = platform_config.platform or platform_key

  local filename = interpolate(url_template, vars)
  local asset_name = filename:match("[^/]+$")

  return asset_name, filename
end

local function fetch_latest_github(config, opts)
  opts = opts or {}
  local stderr = opts.stderr or io.stderr

  local repo = extract_github_repo(config.url)
  if not repo then
    return nil, "could not extract GitHub repo from URL: " .. config.url
  end

  stderr:write("fetching latest release from " .. repo .. "...\n")
  local api_url = "https://api.github.com/repos/" .. repo .. "/releases/latest"
  local release, err = fetch_json(api_url)
  if not release then
    return nil, err
  end

  local version = release.tag_name
  local version_clean = version:gsub("^v", "")

  local version_info = {
    version = version_clean,
    tag = version,
  }

  local sha256sums = try_fetch_sha256sums(repo, version)

  local platforms = {}
  for platform_key, platform_config in pairs(config.platforms) do
    if platform_key == "*" then
      local asset_name = config.url:match("([^/]+)$"):gsub("{version}", version_clean)

      local sha
      if sha256sums then
        sha = sha256sums[asset_name]
      end

      if not sha then
        stderr:write("downloading and hashing " .. asset_name .. "...\n")
        local asset_url = interpolate(config.url, {version = version_clean, tag = version})
        sha, err = fetch_sha256(asset_url)
        if not sha then
          return nil, err
        end
      end

      platforms["*"] = {sha = sha}
    else
      local asset_name, full_url = infer_asset_name(config.url, platform_config, platform_key, version_info)

      local sha
      if sha256sums then
        for name, hash in pairs(sha256sums) do
          if name:find(asset_name, 1, true) or asset_name:find(name, 1, true) then
            sha = hash
            break
          end
        end
      end

      if not sha then
        stderr:write("downloading and hashing " .. asset_name .. " for " .. platform_key .. "...\n")
        sha, err = fetch_sha256(full_url)
        if not sha then
          return nil, "failed to fetch " .. platform_key .. ": " .. err
        end
      else
        stderr:write("found hash for " .. asset_name .. " in SHA256SUMS\n")
      end

      local plat = {sha = sha}
      for k, v in pairs(platform_config) do
        if k ~= "sha" and type(v) ~= "table" then
          plat[k] = v
        end
      end
      platforms[platform_key] = plat
    end
  end

  local result = {
    version = version_clean,
    tag = version,
  }

  for k, v in pairs(config) do
    if k ~= "version" and k ~= "platforms" and k ~= "tag" and k ~= "sha" and type(v) ~= "table" then
      result[k] = v
    end
  end

  result.platforms = platforms

  return result
end

local function check(version_file, output_file, opts)
  opts = opts or {}
  local stderr = opts.stderr or io.stderr

  local ok, config = pcall(dofile, version_file)
  if not ok then
    return nil, "failed to load " .. version_file .. ": " .. tostring(config)
  end

  local latest
  local is_github = config.url and config.url:match("github%.com")

  if is_github then
    stderr:write("checking latest version for " .. version_file .. "...\n")
    local fetch_err
    latest, fetch_err = fetch_latest_github(config, opts)
    if not latest then
      stderr:write("warning: " .. fetch_err .. "\n")
      stderr:write("falling back to current version\n")
      latest = config
      latest._todo = true
    end
  else
    stderr:write("no GitHub URL found, using current version for " .. version_file .. "...\n")
    latest = config
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

local function report(output_dir)
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

    local ok = report(output_dir)
    os.exit(ok and 0 or 1)
  else
    local version_file = arg[1]
    local output_file = arg[2]
    if not version_file or not output_file then
      io.stderr:write("usage: latest.lua <version_file> <output_file>\n")
      io.stderr:write("       latest.lua report <output_dir>\n")
      os.exit(1)
    end

    local ok, err = check(version_file, output_file)
    if not ok then
      io.stderr:write("error: " .. err .. "\n")
      os.exit(1)
    end
  end
end

return {
  extract_github_repo = extract_github_repo,
  parse_sha256sums = parse_sha256sums,
  interpolate = interpolate,
  infer_asset_name = infer_asset_name,
  fetch_latest_github = fetch_latest_github,
  check = check,
  report = report,
}
