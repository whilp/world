#!/usr/bin/env lua
io.stderr:write("install.lua starting\n")
io.stderr:flush()

local cosmo = require("cosmo")
io.stderr:write("loaded cosmo\n")
io.stderr:flush()

local path = require("cosmo.path")
io.stderr:write("loaded path\n")
io.stderr:flush()

local unix = require("cosmo.unix")
io.stderr:write("loaded unix\n")
io.stderr:flush()

local function copy_file_raw(src, dst)
  io.stderr:write("copy_file_raw: " .. src .. " -> " .. dst .. "\n")
  io.stderr:flush()

  -- use shell cp to avoid cosmopolitan memory issues with large APE binaries
  local cmd = string.format('/usr/bin/cp -p "%s" "%s"', src, dst)
  io.stderr:write("copy_file_raw: executing: " .. cmd .. "\n")
  io.stderr:flush()

  local ok = os.execute(cmd)
  io.stderr:write("copy_file_raw: os.execute returned\n")
  io.stderr:flush()

  if not ok then
    return nil, "cp failed"
  end
  return true
end

local function copy_dir_recursive(src, dst)
  local st = unix.stat(src)
  if not st then
    return nil, "source not found: " .. src
  end

  if unix.S_ISDIR(st:mode()) then
    unix.makedirs(dst)
    local dir = unix.opendir(src)
    if not dir then
      return nil, "failed to open directory: " .. src
    end
    for name in dir do
      if name ~= "." and name ~= ".." then
        local ok, err = copy_dir_recursive(path.join(src, name), path.join(dst, name))
        if not ok then return nil, err end
      end
    end
  else
    local ok, err = copy_file_raw(src, dst)
    if not ok then return nil, err end
  end
  return true
end

local function copy_file(source, target_dir, install_type, tool_name)
  local source_name = path.basename(source)
  local dest

  if install_type == "bin" then
    local generic_names = { download = true, binary = true, bin = true }
    if generic_names[source_name] then
      dest = path.join(target_dir, tool_name)
    else
      dest = path.join(target_dir, source_name)
    end
  else
    dest = path.join(target_dir, source_name)
  end

  return copy_file_raw(source, dest)
end

local function copy_dir(source, target_dir)
  return copy_dir_recursive(source, target_dir)
end

local function install(version_file, platform, base_dir, install_type, source)
  io.stderr:write("install: starting\n")
  io.stderr:flush()

  if not version_file or not platform or not base_dir or not install_type or not source then
    return nil, "usage: install.lua <version_file> <platform> <base_dir> <bin|lib> <source>"
  end

  io.stderr:write("install: loading version file\n")
  io.stderr:flush()

  local ok, spec = pcall(dofile, version_file)
  if not ok then
    return nil, "failed to load " .. version_file .. ": " .. tostring(spec)
  end

  io.stderr:write("install: got spec\n")
  io.stderr:flush()

  local plat = spec.platforms[platform] or spec.platforms["*"]
  if not plat then
    return nil, "unknown platform: " .. platform
  end

  local version_dir = spec.version .. "-" .. plat.sha:sub(1, 8)
  local target_dir = path.join(base_dir, version_dir, install_type)
  local tool_name = path.basename(base_dir)

  io.stderr:write("install: makedirs " .. target_dir .. "\n")
  io.stderr:flush()

  unix.makedirs(target_dir)

  io.stderr:write("install: stat source\n")
  io.stderr:flush()

  local stat = unix.stat(source)
  if not stat then
    return nil, "source not found: " .. source
  end

  io.stderr:write("install: copying\n")
  io.stderr:flush()

  local err
  if unix.S_ISDIR(stat:mode()) then
    ok, err = copy_dir(source, target_dir)
  else
    ok, err = copy_file(source, target_dir, install_type, tool_name)
  end

  io.stderr:write("install: copy done\n")
  io.stderr:flush()

  if not ok then
    return nil, err
  end

  -- create symlink
  local link_path = path.join(base_dir, install_type)
  unix.unlink(link_path)
  unix.symlink(path.join(version_dir, install_type), link_path)

  io.stderr:write("install: done\n")
  io.stderr:flush()

  return true
end

local function main(version_file, platform, base_dir, install_type, source)
  io.stderr:write("main: args received\n")
  io.stderr:flush()

  if not version_file or not platform or not base_dir or not install_type or not source then
    return nil, "usage: install.lua <version_file> <platform> <base_dir> <bin|lib> <source>"
  end

  io.stderr:write("main: CI=" .. tostring(os.getenv("CI")) .. "\n")
  io.stderr:flush()

  -- skip unveil in CI environments (can cause bus errors with APE binaries)
  if not os.getenv("CI") then
    local lua_bin = arg[-1] or arg[0]
    if lua_bin then unix.unveil(lua_bin, "rx") end

    unix.unveil(version_file, "r")
    unix.unveil(source, "r")
    unix.unveil(base_dir, "rwc")
    unix.unveil("/usr", "rx")
    unix.unveil("/bin", "rx")
    unix.unveil(nil, nil)
  end

  io.stderr:write("main: calling install\n")
  io.stderr:flush()

  return install(version_file, platform, base_dir, install_type, source)
end

if not pcall(debug.getlocal, 4, 1) then
  io.stderr:write("calling main\n")
  io.stderr:flush()
  local ok, err = main(...)
  io.stderr:write("main returned\n")
  io.stderr:flush()
  if not ok then
    io.stderr:write("error: " .. err .. "\n")
    os.exit(1)
  end
end

return {
  copy_file = copy_file,
  copy_dir = copy_dir,
  install = install,
}
