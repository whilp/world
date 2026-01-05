#!/usr/bin/env lua
-- teal ignore: type annotations needed

local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local common = require("checker.common")

local function fetch_json(url)
  local status, _, body = cosmo.Fetch(url, {
    headers = {["User-Agent"] = "curl/8.0", ["Accept"] = "application/vnd.github+json"},
  })
  if status ~= 200 then
    return nil, "fetch failed: " .. tostring(status)
  end
  return cosmo.DecodeJson(body)
end

local function extract_github_repo(url)
  local owner, repo = url:match("github%.com/([^/]+)/([^/]+)")
  if owner and repo then
    return string.format("%s/%s", owner, repo)
  end
  return nil
end

local function check_latest_version(config)
  local repo = extract_github_repo(config.url)
  if not repo then
    return nil, "not a GitHub URL"
  end

  local api_url = "https://api.github.com/repos/" .. repo .. "/releases/latest"
  local release, err = fetch_json(api_url)
  if not release then
    return nil, err
  end

  local version = release.tag_name
  local version_clean = version:gsub("^v", "")

  return version_clean
end

local function main(version_file)
  if not version_file then
    io.stderr:write("usage: check-update.lua <version_file>\n")
    return 1
  end

  local content = cosmo.Slurp(version_file)
  if not content then
    return common.write_result("fail", "could not read file", "", "")
  end

  local _, skip_reason = common.check_first_lines(version_file, {
    shebangs = {},
    ignore = "^%-%-%s*update%s+ignore:%s*(.*)",
  })

  if skip_reason then
    return common.write_result("skip", skip_reason, "", "")
  end

  local chunk, err = load(content, version_file)
  if not chunk then
    return common.write_result("fail", "could not parse: " .. tostring(err), "", "")
  end

  local ok, config = pcall(chunk)
  if not ok then
    return common.write_result("fail", "could not load: " .. tostring(config), "", "")
  end

  local current_version = config.version
  if not current_version then
    return common.write_result("fail", "no version field", "", "")
  end

  local latest_version, check_err = check_latest_version(config)

  if not latest_version then
    return common.write_result("fail", check_err or "could not check", "", "")
  elseif latest_version == current_version then
    return common.write_result("pass", current_version, "", "")
  else
    return common.write_result("skip", current_version .. " -> " .. latest_version, "", "")
  end
end

if cosmo.is_main() then
  os.exit(main(...))
end
