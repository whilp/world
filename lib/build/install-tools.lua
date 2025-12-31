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
  -- read manifest from .extracted
  local manifest_path = path.join(tool_dir, ".extracted")
  local ok, manifest = pcall(dofile, manifest_path)
  if not ok or not manifest then
    io.stderr:write("  skipping " .. tool_name .. " (no manifest)\n")
    return
  end

  local version = manifest.version or "0.0.0"
  local sha = (manifest.sha or "00000000"):sub(1, 8)
  local install_dir = path.join(install_base, tool_name, version .. "-" .. sha)

  print("  installing " .. tool_name .. " " .. version .. "-" .. sha)
  unix.makedirs(install_dir)

  -- nvim and gh have full directory structures
  if tool_name == "nvim" or tool_name == "gh" then
    for _, subdir in ipairs({"bin", "lib", "share", "libexec"}) do
      local src = path.join(tool_dir, subdir)
      if unix.stat(src) then
        copy_recursive(src, path.join(install_dir, subdir))
      end
    end
  else
    -- simple tools: just copy the binary
    local bin_dir = path.join(install_dir, "bin")
    unix.makedirs(bin_dir)

    local src_bin = path.join(tool_dir, "bin", tool_name)
    if not unix.stat(src_bin) then
      src_bin = path.join(tool_dir, tool_name)
    end

    if unix.stat(src_bin) then
      local dest = path.join(bin_dir, tool_name)
      execute({"cp", "-p", src_bin, dest})
      unix.chmod(dest, tonumber("755", 8))
    end
  end
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
