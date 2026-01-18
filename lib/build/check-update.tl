#!/usr/bin/env lua

local cosmo = require("cosmo")
local getopt = require("cosmo.getopt")
local common = require("checker.common")
local fetch = require("cosmic.fetch")

local record GithubAsset
  name: string
  browser_download_url: string
end

local record GithubRelease
  tag_name: string
  assets: {GithubAsset}
end

local type PlatformConfig = {string:string}

local record Config
  version: string
  format: string
  strip_components: number
  strip_prefix: string
  url: string
  platforms: {string:PlatformConfig}
end

local function fetch_json(url: string): any, string
  local result = fetch.Fetch(url, {
    headers = {["User-Agent"] = "curl/8.0", ["Accept"] = "application/vnd.github+json"},
  })
  if not result.ok then
    return nil, "fetch failed: " .. result.error
  end
  if result.status ~= 200 then
    return nil, "fetch failed: HTTP " .. tostring(result.status)
  end
  return cosmo.DecodeJson(result.body)
end

local function fetch_text(url: string): string, string
  local result = fetch.Fetch(url, {
    headers = {["User-Agent"] = "curl/8.0"},
  })
  if not result.ok then
    return nil, "fetch failed: " .. result.error
  end
  if result.status ~= 200 then
    return nil, "fetch failed: HTTP " .. tostring(result.status)
  end
  return result.body
end

local function extract_github_repo(url: string): string
  local owner, repo = url:match("github%.com/([^/]+)/([^/]+)")
  if owner and repo then
    return string.format("%s/%s", owner, repo)
  end
  return nil
end

local function check_latest_release(config: Config): string, GithubRelease, string
  local repo = extract_github_repo(config.url)
  if not repo then
    return nil, nil, "not a GitHub URL"
  end

  local api_url = "https://api.github.com/repos/" .. repo .. "/releases/latest"
  local release_data, err = fetch_json(api_url)
  if not release_data then
    return nil, nil, err
  end

  local release = release_data as GithubRelease
  local version = release.tag_name
  local version_clean = version:gsub("^v", "")

  return version_clean, release
end

local function find_checksum_asset(release: GithubRelease): string
  for _, asset in ipairs(release.assets or {}) do
    local name = asset.name:lower()
    if name:match("checksum") or name:match("sha256") then
      return asset.browser_download_url
    end
  end
  return nil
end

local function parse_checksums(text: string): {string:string}
  local checksums: {string:string} = {}
  for line in text:gmatch("[^\n]+") do
    local sha, filename = line:match("^(%x+)%s+(.+)$")
    if sha and filename then
      checksums[filename] = sha
    end
  end
  return checksums
end

local function build_filename(template: string, version: string, platform_config: PlatformConfig): string
  local filename = template:gsub("{version}", version)
  for key, value in pairs(platform_config as {string:string}) do
    filename = filename:gsub("{" .. key .. "}", value)
  end
  return filename
end

local function generate_updated_version(new_version: string, checksums: {string:string}, config: Config): string, string
  local url_template = config.url
  local basename_template = url_template:match("/([^/]+)$")

  config.version = new_version
  for platform, platform_config in pairs(config.platforms) do
    local filename = build_filename(basename_template, new_version, platform_config)
    local sha = checksums[filename]
    if not sha then
      return nil, string.format("no checksum for %s (platform %s)", filename, platform)
    end
    platform_config.sha = sha
  end

  return "return " .. cosmo.EncodeLua(config as any, {pretty = true}) .. "\n"
end

local function apply_update(update_ok_file: string, version_file: string): number
  local content = cosmo.Slurp(update_ok_file)
  if not content then
    return 1
  end
  if not content:match("^fail:") then
    return 0
  end
  local stdout = content:match("\n## stdout\n\n(.-)\n## stderr")
  if not stdout or stdout == "" then
    return 0
  end
  local f = io.open(version_file, "w")
  if not f then
    return 1
  end
  f:write(stdout)
  f:close()
  return 0
end

local function main(...: string): number
  local args = {...}
  local apply_file: string = nil
  local longopts = {{"apply", "required"}}
  local parser = getopt.new(args, "", longopts)

  while true do
    local opt, optarg = parser:next()
    if not opt then break end
    if opt == "apply" then
      apply_file = optarg
    elseif opt == "?" then
      io.stderr:write("usage: check-update.lua [--apply <update_ok_file>] <version_file>\n")
      return 1
    end
  end

  local remaining = parser:remaining()
  local version_file = remaining[1]

  if apply_file then
    if not version_file then
      io.stderr:write("usage: check-update.lua --apply <update_ok_file> <version_file>\n")
      return 1
    end
    return apply_update(apply_file, version_file)
  end

  if not version_file then
    io.stderr:write("usage: check-update.lua <version_file>\n")
    return 1
  end

  local content = cosmo.Slurp(version_file)
  if not content then
    return common.write_result("fail", "could not read file", "", "", version_file)
  end

  local _, skip_reason = common.check_first_lines(version_file, {
    shebangs = {},
    ignore = "^%-%-%s*update%s+ignore:%s*(.*)",
  })

  if skip_reason then
    return common.write_result("skip", skip_reason, "", "", version_file)
  end

  local chunk, err = load(content, version_file)
  if not chunk then
    return common.write_result("fail", "could not parse: " .. tostring(err), "", "", version_file)
  end

  local ok, config_result = pcall(chunk)
  if not ok then
    return common.write_result("fail", "could not load: " .. tostring(config_result), "", "", version_file)
  end

  local config = config_result as Config
  local current_version = config.version
  if not current_version then
    return common.write_result("fail", "no version field", "", "", version_file)
  end

  local latest_version, release, check_err = check_latest_release(config)

  if not latest_version then
    return common.write_result("fail", check_err or "could not check", "", "", version_file)
  elseif latest_version == current_version then
    return common.write_result("pass", current_version, "", "", version_file)
  end

  -- update available - generate updated version.lua
  local checksum_url = find_checksum_asset(release)
  if not checksum_url then
    return common.write_result("fail", current_version .. " -> " .. latest_version .. " (no checksums)", "", "", version_file)
  end

  local checksum_text, fetch_err = fetch_text(checksum_url)
  if not checksum_text then
    return common.write_result("fail", current_version .. " -> " .. latest_version .. " (" .. tostring(fetch_err) .. ")", "", "", version_file)
  end

  local checksums = parse_checksums(checksum_text)
  local updated_content, gen_err = generate_updated_version(latest_version, checksums, config)
  if not updated_content then
    return common.write_result("fail", current_version .. " -> " .. latest_version .. " (" .. tostring(gen_err) .. ")", "", "", version_file)
  end

  common.write_result("fail", current_version .. " -> " .. latest_version, updated_content, "", version_file)
  return 0
end

if cosmo.is_main() then
  os.exit(main(...))
end
