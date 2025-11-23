#!/usr/bin/env luajit

local function printf(fmt, ...)
  io.write(string.format(fmt, ...))
  io.flush()
end

local function errorf(fmt, ...)
  io.stderr:write(string.format(fmt, ...))
  io.stderr:flush()
  os.exit(1)
end

local function exec(cmd, opts)
  opts = opts or {}
  if not opts.quiet then
    printf("+ %s\n", cmd)
  end
  local ret = os.execute(cmd)
  if ret ~= 0 and not opts.allow_failure then
    errorf("command failed: %s\n", cmd)
  end
  return ret == 0
end

local function capture(cmd)
  local handle = io.popen(cmd)
  if not handle then
    errorf("failed to run: %s\n", cmd)
  end
  local result = handle:read("*a")
  handle:close()
  return result:gsub("%s+$", "")
end

local function mkdir_p(path)
  exec(string.format("mkdir -p %s", path), {quiet = true})
end

local function file_exists(path)
  local f = io.open(path, "r")
  if f then
    f:close()
    return true
  end
  return false
end

local function read_file(path)
  local f = io.open(path, "r")
  if not f then
    errorf("failed to open file: %s\n", path)
  end
  local content = f:read("*a")
  f:close()
  return content
end

local function write_file(path, content)
  local f = io.open(path, "w")
  if not f then
    errorf("failed to open file for writing: %s\n", path)
  end
  f:write(content)
  f:close()
end

local function load_pack_lock(lock_file)
  if not file_exists(lock_file) then
    errorf("pack lock file not found: %s\n", lock_file)
  end

  local content = read_file(lock_file)
  local json = require("dkjson")
  local data, pos, err = json.decode(content)

  if err then
    errorf("failed to parse pack lock file: %s\n", err)
  end

  return data
end

local function get_script_dir()
  local str = debug.getinfo(1, "S").source:sub(2)
  return str:match("^(.*/)") or "./"
end

local function detect_platform()
  local os_name = capture("uname -s"):lower()
  local arch = capture("uname -m")

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

local function download_nvim_nightly(config)
  printf("downloading nvim nightly for %s\n", config.platform.platform)

  local asset_name = string.format("nvim-%s-%s.tar.gz",
    config.platform.is_darwin and "macos" or "linux",
    config.platform.nvim_arch)

  local upstream_url = string.format(
    "https://github.com/neovim/neovim/releases/download/nightly/%s",
    asset_name)

  printf("downloading from %s\n", upstream_url)
  local download_path = config.temp_dir .. "/" .. asset_name
  exec(string.format("curl -L -o %s %s", download_path, upstream_url))

  printf("extracting archive\n")
  exec(string.format("cd %s && tar -xzf %s", config.temp_dir, asset_name))

  local extracted_dir = capture(string.format(
    "cd %s && tar -tzf %s | head -1 | cut -f1 -d/",
    config.temp_dir, asset_name))

  config.extracted_dir = config.temp_dir .. "/" .. extracted_dir
  config.asset_name = asset_name

  printf("extracted to %s\n", config.extracted_dir)
end

local function bundle_plugins(config)
  printf("bundling vim.pack plugins\n")

  local pack_dir = config.extracted_dir .. "/share/nvim/site/pack/core/opt"
  mkdir_p(pack_dir)

  local plugins = config.pack_lock.plugins

  for name, info in pairs(plugins) do
    printf("cloning %s at %s\n", name, info.rev)
    local plugin_dir = pack_dir .. "/" .. name

    local clone_cmd = string.format(
      "git clone --depth 1 --branch %s %s %s",
      info.rev, info.src, plugin_dir)

    if not exec(clone_cmd, {allow_failure = true}) then
      printf("branch clone failed, trying checkout\n")
      exec(string.format("git clone %s %s", info.src, plugin_dir))
      exec(string.format("cd %s && git checkout %s", plugin_dir, info.rev))
    end

    exec(string.format("rm -rf %s/.git", plugin_dir))
  end

  printf("generating helptags\n")
  local nvim_bin = config.extracted_dir .. "/bin/nvim"
  exec(string.format("%s --headless +'helptags ALL' +qa", nvim_bin))
end

local function repackage_nvim(config)
  printf("repackaging nvim with plugins\n")

  local platform_dir = config.output_dir .. "/" .. config.platform.platform
  mkdir_p(platform_dir)

  local build_date = capture("date +%Y.%m.%d")
  local binary_name = string.format("nvim-%s-%s", build_date, config.platform.platform)
  local tarball_name = binary_name .. ".tar.gz"
  local tarball_path = platform_dir .. "/" .. tarball_name

  printf("creating tarball %s\n", tarball_name)

  local extracted_basename = capture(string.format("basename %s", config.extracted_dir))
  local staging_dir = config.temp_dir .. "/staging"
  mkdir_p(staging_dir)

  exec(string.format("cp -R %s %s/%s", config.extracted_dir, staging_dir, binary_name))

  if config.platform.is_darwin then
    exec(string.format(
      "cd %s && find %s -print0 | sort -z | tar -cf - --null -T - | gzip -n > %s",
      staging_dir, binary_name, tarball_path))
  else
    exec(string.format(
      "cd %s && tar --sort=name --owner=0 --group=0 --numeric-owner -czf %s %s",
      staging_dir, tarball_path, binary_name))
  end

  exec(string.format(
    "cd %s && ln -sf %s nvim-latest-%s.tar.gz",
    platform_dir, tarball_name, config.platform.platform))

  printf("calculating sha256\n")
  local checksum_cmd = "shasum -a 256"
  if not exec("command -v shasum >/dev/null 2>&1", {quiet = true, allow_failure = true}) then
    checksum_cmd = "sha256sum"
  end

  local checksum = capture(string.format(
    "cd %s && %s %s | cut -d' ' -f1",
    platform_dir, checksum_cmd, tarball_name))

  write_file(
    platform_dir .. "/" .. tarball_name .. ".sha256",
    string.format("%s  %s\n", checksum, tarball_name))

  printf("testing binary\n")
  local nvim_bin = staging_dir .. "/" .. binary_name .. "/bin/nvim"
  exec(nvim_bin .. " --version")
  exec(nvim_bin .. " --headless +'lua print(\"nvim test successful\")' +qa")

  printf("verifying plugins\n")
  for name, _ in pairs(config.pack_lock.plugins) do
    exec(string.format(
      "%s --headless +'packadd %s' +'lua print(\"plugin %s loaded\")' +qa",
      nvim_bin, name, name))
  end

  local size = capture("du -h " .. nvim_bin .. " | cut -f1")
  printf("binary size: %s\n", size)

  printf("build complete!\n")
  printf("tarball: %s/%s\n", platform_dir, tarball_name)
  printf("sha256: %s\n", checksum)
end

local function main()
  local script_dir = get_script_dir()
  local lock_file = script_dir .. "/../.config/nvim/nvim-pack-lock.json"

  local pack_lock = load_pack_lock(lock_file)

  local temp_dir = capture("mktemp -d")
  local output_dir = os.getenv("OUTPUT_DIR") or (script_dir .. "/../dist/nvim")

  local config = {
    pack_lock = pack_lock,
    temp_dir = temp_dir,
    output_dir = output_dir,
    platform = detect_platform(),
  }

  local ok, err = pcall(function()
    download_nvim_nightly(config)
    bundle_plugins(config)
    repackage_nvim(config)
  end)

  if not ok then
    errorf("build failed: %s\n", err)
  end

  printf("cleaning up\n")
  exec("rm -rf " .. config.temp_dir)
end

main()
