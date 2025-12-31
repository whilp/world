-- lib/build/install-tools.lua - install tools from binaries zip to versioned directories
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("spawn").spawn

local function execute(args)
  local handle, err = spawn(args)
  if not handle then
    return nil, err
  end
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    return nil, "command failed with exit " .. exit_code
  end
  return true
end

local function copy_recursive(src, dest)
  local st = unix.stat(src)
  if not st then return end

  if unix.S_ISDIR(st:mode()) then
    unix.makedirs(dest)
    for name in unix.opendir(src) do
      if name ~= "." and name ~= ".." then
        copy_recursive(path.join(src, name), path.join(dest, name))
      end
    end
  else
    execute({"cp", "-p", src, dest})
  end
end

local function install_tool(tool_dir, tool_name, install_base)
  -- get version from bin symlink target (e.g., "0.35.0-abc12345/bin")
  local bin_symlink = path.join(tool_dir, "bin")
  local target = unix.readlink(bin_symlink)
  if not target then
    io.stderr:write("  skipping " .. tool_name .. " (no bin symlink)\n")
    return
  end

  -- parse "version-sha/bin" to get version directory
  local version_dir = target:match("^([^/]+)/bin$")
  if not version_dir then
    io.stderr:write("  skipping " .. tool_name .. " (invalid symlink: " .. target .. ")\n")
    return
  end

  local install_dir = path.join(install_base, tool_name, version_dir)
  local src_dir = path.join(tool_dir, version_dir)

  print("  installing " .. tool_name .. " " .. version_dir)
  copy_recursive(src_dir, install_dir)
end

-- CLI: install-tools.lua <binaries_zip> <platform> <output_dir>
local binaries_zip = arg[1]
local platform = arg[2]
local output_dir = arg[3]

if not binaries_zip or not platform or not output_dir then
  io.stderr:write("usage: install-tools.lua <binaries_zip> <platform> <output_dir>\n")
  os.exit(1)
end

local install_base = path.join(output_dir, "home", ".local", "share")
local temp_dir = path.join(output_dir, "temp-binaries")

-- extract zip
unix.makedirs(temp_dir)
local ok, err = execute({"unzip", "-q", binaries_zip, "-d", temp_dir})
if not ok then
  io.stderr:write("error: failed to unzip: " .. tostring(err) .. "\n")
  os.exit(1)
end

-- install each tool
unix.makedirs(install_base)
for name in unix.opendir(temp_dir) do
  if name ~= "." and name ~= ".." then
    local tool_dir = path.join(temp_dir, name, platform)
    if unix.stat(tool_dir) then
      install_tool(tool_dir, name, install_base)
    end
  end
end

-- cleanup
unix.rmrf(temp_dir)
