#!/usr/bin/env luajit

local M = {}

function M.printf(fmt, ...)
  io.write(string.format(fmt, ...))
  io.flush()
end

function M.errorf(fmt, ...)
  io.stderr:write(string.format(fmt, ...))
  io.stderr:flush()
  os.exit(1)
end

function M.exec(cmd, opts)
  opts = opts or {}
  if not opts.quiet then
    M.printf("+ %s\n", cmd)
  end
  local ret = os.execute(cmd)
  if ret ~= 0 and not opts.allow_failure then
    M.errorf("command failed: %s\n", cmd)
  end
  return ret == 0
end

function M.capture(cmd)
  local handle = io.popen(cmd)
  if not handle then
    M.errorf("failed to run: %s\n", cmd)
  end
  local result = handle:read("*a")
  handle:close()
  return result:gsub("%s+$", "")
end

function M.mkdir_p(path)
  M.exec(string.format("mkdir -p %s", path), {quiet = true})
end

function M.file_exists(path)
  local f = io.open(path, "r")
  if f then
    f:close()
    return true
  end
  return false
end

function M.read_file(path)
  local f = io.open(path, "r")
  if not f then
    M.errorf("failed to open file: %s\n", path)
  end
  local content = f:read("*a")
  f:close()
  return content
end

function M.write_file(path, content)
  local f = io.open(path, "w")
  if not f then
    M.errorf("failed to open file for writing: %s\n", path)
  end
  f:write(content)
  f:close()
end

function M.get_script_dir()
  local str = debug.getinfo(2, "S").source:sub(2)
  return str:match("^(.*/)") or "./"
end

function M.detect_platform()
  local os_name = M.capture("uname -s"):lower()
  local arch = M.capture("uname -m")

  if arch == "aarch64" then
    arch = "arm64"
  elseif arch == "x86_64" then
    arch = "x64"
  end

  local nvim_arch = arch
  if arch == "x64" then
    nvim_arch = "x86_64"
  end

  return {
    os = os_name,
    arch = arch,
    nvim_arch = nvim_arch,
    platform = os_name .. "-" .. arch,
    is_darwin = os_name == "darwin",
    is_linux = os_name == "linux",
  }
end

function M.create_tarball(staging_dir, binary_name, output_path, platform, source_date_epoch)
  M.printf("creating tarball %s\n", output_path)

  if platform.is_darwin then
    M.exec(string.format(
      "cd %s && find %s -print0 | sort -z | tar -cf - --null -T - | gzip -n > %s",
      staging_dir, binary_name, output_path))
  else
    local mtime_opt = source_date_epoch and string.format("--mtime='@%s'", source_date_epoch) or ""
    local pax_opt = "--pax-option=exthdr.name=%%d/PaxHeaders/%%f,delete=atime,delete=ctime"
    M.exec(string.format(
      "cd %s && tar --sort=name %s --owner=0 --group=0 --numeric-owner %s -cf - %s | gzip -n > %s",
      staging_dir, mtime_opt, pax_opt, binary_name, output_path))
  end
end

function M.calculate_sha256(file_path)
  local dir = file_path:match("^(.*/)") or "./"
  local filename = file_path:match("([^/]+)$")

  local checksum_cmd = "shasum -a 256"
  if not M.exec("command -v shasum >/dev/null 2>&1", {quiet = true, allow_failure = true}) then
    checksum_cmd = "sha256sum"
  end

  local checksum = M.capture(string.format(
    "cd %s && %s %s | cut -d' ' -f1",
    dir, checksum_cmd, filename))

  M.write_file(
    file_path .. ".sha256",
    string.format("%s  %s\n", checksum, filename))

  return checksum
end

return M
