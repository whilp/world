#!/usr/bin/env lua
local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

local function fetch_json(url)
  local status, headers, body = cosmo.Fetch(url, {
    headers = {["User-Agent"] = "curl/8.0", ["Accept"] = "application/vnd.github+json"},
  })
  if not status then
    return nil, "fetch failed: " .. tostring(headers or "unknown error")
  end
  if status ~= 200 then
    return nil, "fetch status " .. tostring(status)
  end
  return cosmo.DecodeJson(body)
end

local function parse_action_line(line)
  local uses = line:match("%s*uses:%s*(.+)%s*$")
  if not uses then return nil end

  local action, ref_and_comment = uses:match("^([^@]+)@(.+)$")
  if not action then return nil end

  local ref, comment = ref_and_comment:match("^(%S+)%s*#%s*(.+)$")
  if not ref then
    ref = ref_and_comment
    comment = nil
  end

  return {
    action = action,
    ref = ref,
    comment = comment,
  }
end

local function find_actions_in_file(file_path)
  local f = io.open(file_path, "r")
  if not f then
    return nil, "failed to open file: " .. file_path
  end

  local actions = {}
  local line_num = 0

  for line in f:lines() do
    line_num = line_num + 1
    local parsed = parse_action_line(line)
    if parsed then
      local found = false
      for _, existing in ipairs(actions) do
        if existing.action == parsed.action then
          found = true
          table.insert(existing.locations, {
            line_num = line_num,
            ref = parsed.ref,
            comment = parsed.comment,
            full_line = line,
          })
          break
        end
      end

      if not found then
        table.insert(actions, {
          action = parsed.action,
          locations = {{
            line_num = line_num,
            ref = parsed.ref,
            comment = parsed.comment,
            full_line = line,
          }},
        })
      end
    end
  end

  f:close()
  return actions
end

local function find_actions_in_dir(dir_path)
  local workflow_dir = path.join(dir_path, ".github", "workflows")
  local st = unix.stat(workflow_dir)
  if not st then
    return nil, "workflow directory not found: " .. workflow_dir
  end

  local all_actions = {}
  local dir = unix.opendir(workflow_dir)
  if not dir then
    return nil, "failed to open directory: " .. workflow_dir
  end

  for name in dir do
    if name:match("%.ya?ml$") then
      local file_path = path.join(workflow_dir, name)
      local actions, err = find_actions_in_file(file_path)
      if not actions then
        io.stderr:write("warning: " .. err .. "\n")
      else
        for _, action_info in ipairs(actions) do
          action_info.file = name
          action_info.file_path = file_path
          table.insert(all_actions, action_info)
        end
      end
    end
  end

  return all_actions
end

local function get_latest_release(owner, repo)
  local url = string.format("https://api.github.com/repos/%s/%s/releases/latest", owner, repo)
  local release, err = fetch_json(url)
  if not release then
    return nil, err
  end

  return {
    tag = release.tag_name,
    sha = release.target_commitish,
    published_at = release.published_at,
  }
end

local function get_tag_info(owner, repo, tag)
  local url = string.format("https://api.github.com/repos/%s/%s/git/refs/tags/%s", owner, repo, tag)
  local tag_ref, err = fetch_json(url)
  if not tag_ref then
    return nil, err
  end

  if tag_ref.object and tag_ref.object.type == "commit" then
    return {
      tag = tag,
      sha = tag_ref.object.sha,
    }
  end

  if tag_ref.object and tag_ref.object.type == "tag" then
    local tag_url = tag_ref.object.url
    local tag_obj, tag_err = fetch_json(tag_url)
    if not tag_obj then
      return nil, tag_err
    end

    return {
      tag = tag,
      sha = tag_obj.object.sha,
    }
  end

  return nil, "unexpected tag object type"
end

local function get_latest_for_action(action)
  local owner, repo = action:match("^([^/]+)/([^/]+)$")
  if not owner or not repo then
    return nil, "invalid action format: " .. action
  end

  local release, err = get_latest_release(owner, repo)
  if not release then
    return nil, err
  end

  local tag_info, tag_err = get_tag_info(owner, repo, release.tag)
  if not tag_info then
    return nil, tag_err
  end

  return {
    action = action,
    tag = release.tag,
    sha = tag_info.sha,
    published_at = release.published_at,
  }
end

local function update_action_in_file(file_path, action, old_ref, new_sha, new_tag)
  local f = io.open(file_path, "r")
  if not f then
    return nil, "failed to open file: " .. file_path
  end

  local lines = {}
  for line in f:lines() do
    table.insert(lines, line)
  end
  f:close()

  local updated = false
  for i, line in ipairs(lines) do
    local parsed = parse_action_line(line)
    if parsed and parsed.action == action and parsed.ref == old_ref then
      local indent = line:match("^(%s*)uses:")
      local new_line = string.format("%suses: %s@%s # %s", indent, action, new_sha, new_tag)
      lines[i] = new_line
      updated = true
    end
  end

  if not updated then
    return nil, "action not found in file"
  end

  local out = io.open(file_path, "w")
  if not out then
    return nil, "failed to write file: " .. file_path
  end

  for _, line in ipairs(lines) do
    out:write(line .. "\n")
  end
  out:close()

  return true
end

local function print_usage()
  io.stderr:write([[
usage: latest.lua [options] [directory]

options:
  -u, --update    update workflow files with latest versions
  -h, --help      show this help message

if no directory is specified, uses current directory
]])
end

local function main(...)
  local args = {...}
  local update = false
  local dir_path = "."

  for i, arg in ipairs(args) do
    if arg == "-u" or arg == "--update" then
      update = true
    elseif arg == "-h" or arg == "--help" then
      print_usage()
      return true
    elseif not arg:match("^-") then
      dir_path = arg
    else
      io.stderr:write("error: unknown option: " .. arg .. "\n")
      print_usage()
      return nil
    end
  end

  io.stderr:write("scanning workflow files in " .. dir_path .. "...\n")
  local actions, err = find_actions_in_dir(dir_path)
  if not actions then
    io.stderr:write("error: " .. err .. "\n")
    return nil
  end

  if #actions == 0 then
    io.stderr:write("no github actions found\n")
    return true
  end

  for _, action_info in ipairs(actions) do
    io.stderr:write("\naction: " .. action_info.action .. "\n")

    local latest, latest_err = get_latest_for_action(action_info.action)
    if not latest then
      io.stderr:write("  error: " .. latest_err .. "\n")
    else
      io.stderr:write("  latest: " .. latest.tag .. " (" .. latest.sha:sub(1, 12) .. "...)\n")

      local needs_update = false
      local old_sha = nil

      for _, loc in ipairs(action_info.locations) do
        local current_sha = loc.ref
        local current_tag = loc.comment or "unknown"

        io.stderr:write("  current: " .. current_tag .. " (" .. current_sha:sub(1, 12) .. "...)")

        if current_sha == latest.sha then
          io.stderr:write(" [up to date]\n")
        else
          io.stderr:write(" [update available]\n")
          needs_update = true
          old_sha = current_sha
        end
      end

      if update and needs_update and old_sha then
        io.stderr:write("  updating all occurrences in " .. action_info.file_path .. "...\n")
        local ok, update_err = update_action_in_file(
          action_info.file_path,
          action_info.action,
          old_sha,
          latest.sha,
          latest.tag
        )
        if not ok then
          io.stderr:write("  error updating: " .. update_err .. "\n")
        else
          io.stderr:write("  updated successfully\n")
        end
      end
    end
  end

  return true
end

if not pcall(debug.getlocal, 4, 1) then
  local ok = main(...)
  if not ok then
    os.exit(1)
  end
end
