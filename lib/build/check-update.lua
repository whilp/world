#!/usr/bin/env lua

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

local function main(version_file, output_file)
  if not version_file or not output_file then
    io.stderr:write("usage: check-update.lua <version_file> <output_file>\n")
    return 1
  end

  local content = cosmo.Slurp(version_file)
  if not content then
    unix.makedirs(path.dirname(output_file))
    cosmo.Barf(output_file, common.format_output("fail", "could not read file", "", ""))
    return 1
  end

  local _, skip_reason = common.check_first_lines(version_file, {
    shebangs = {},
    ignore = "^%-%-%s*update%s+ignore:%s*(.*)",
  })

  if skip_reason then
    unix.makedirs(path.dirname(output_file))
    cosmo.Barf(output_file, common.format_output("skip", skip_reason, "", ""))
    return 0
  end

  local chunk, err = load(content, version_file)
  if not chunk then
    unix.makedirs(path.dirname(output_file))
    cosmo.Barf(output_file, common.format_output("fail", "could not parse: " .. tostring(err), "", ""))
    return 1
  end

  local ok, config = pcall(chunk)
  if not ok then
    unix.makedirs(path.dirname(output_file))
    cosmo.Barf(output_file, common.format_output("fail", "could not load: " .. tostring(config), "", ""))
    return 1
  end

  local current_version = config.version
  if not current_version then
    unix.makedirs(path.dirname(output_file))
    cosmo.Barf(output_file, common.format_output("fail", "no version field", "", ""))
    return 1
  end

  local latest_version, check_err = check_latest_version(config)

  local status, message, stdout, stderr
  if not latest_version then
    status = "fail"
    message = check_err or "could not check"
    stdout = ""
    stderr = ""
  elseif latest_version == current_version then
    status = "pass"
    message = current_version
    stdout = ""
    stderr = ""
  else
    status = "skip"
    message = current_version .. " -> " .. latest_version
    stdout = ""
    stderr = ""
  end

  unix.makedirs(path.dirname(output_file))
  cosmo.Barf(output_file, common.format_output(status, message, stdout, stderr))

  return 0
end

if cosmo.is_main() then
  os.exit(main(...))
end
