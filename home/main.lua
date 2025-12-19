local function detect_platform()
  local uname_s = io.popen("uname -s"):read("*l")
  local uname_m = io.popen("uname -m"):read("*l")

  if not uname_s or not uname_m then
    return nil, "failed to detect platform"
  end

  local os_name = uname_s:lower()
  local arch = uname_m:lower()

  local platform_os
  if os_name:match("darwin") then
    platform_os = "darwin"
  elseif os_name:match("linux") then
    platform_os = "linux"
  else
    return nil, "unsupported OS: " .. uname_s
  end

  local platform_arch
  if arch == "arm64" or arch == "aarch64" then
    platform_arch = "arm64"
  elseif arch == "x86_64" or arch == "amd64" then
    platform_arch = "x86_64"
  else
    return nil, "unsupported architecture: " .. uname_m
  end

  return platform_os .. "-" .. platform_arch
end

local function cmd_unpack()
  local f = io.open("/zip/dotfiles.zip", "rb")
  if not f then
    io.stderr:write("error: embedded dotfiles not found\n")
    os.exit(1)
  end
  local content = f:read("*a")
  f:close()
  io.stdout:write(content)
end

local function cmd_install()
  local dest = os.getenv("HOME")
  if not dest then
    io.stderr:write("error: HOME not set\n")
    os.exit(1)
  end

  local platform, err = detect_platform()
  if not platform then
    io.stderr:write("error: " .. err .. "\n")
    os.exit(1)
  end

  io.stderr:write("platform: " .. platform .. "\n")

  io.stderr:write("extracting dotfiles...\n")
  local dotfiles_tmp = dest .. "/.dotfiles.zip.tmp"
  local df = io.open("/zip/dotfiles.zip", "rb")
  if not df then
    io.stderr:write("error: embedded dotfiles not found\n")
    os.exit(1)
  end
  local dtf = io.open(dotfiles_tmp, "wb")
  if not dtf then
    io.stderr:write("error: failed to write dotfiles\n")
    os.exit(1)
  end
  dtf:write(df:read("*a"))
  df:close()
  dtf:close()

  os.execute(string.format("unzip -q -o '%s' -d '%s'", dotfiles_tmp, dest))
  os.remove(dotfiles_tmp)

  io.stderr:write("extracting binaries for " .. platform .. "...\n")
  local binaries_tmp = dest .. "/.binaries.zip.tmp"
  local bf = io.open("/zip/binaries.zip", "rb")
  if not bf then
    io.stderr:write("warning: no binaries embedded\n")
  else
    local btf = io.open(binaries_tmp, "wb")
    if not btf then
      io.stderr:write("error: failed to write binaries\n")
      os.exit(1)
    end
    btf:write(bf:read("*a"))
    bf:close()
    btf:close()

    local extract_dir = dest .. "/.local/share/home-binaries"
    os.execute("mkdir -p " .. extract_dir)

    local cmd = string.format(
      "unzip -q -o '%s' -d '%s' '*/%s/*' 2>/dev/null || true",
      binaries_tmp,
      extract_dir,
      platform
    )
    os.execute(cmd)
    os.remove(binaries_tmp)

    io.stderr:write("creating symlinks...\n")
    local bin_dir = dest .. "/.local/bin"
    os.execute("mkdir -p " .. bin_dir)

    local binaries = {
      "nvim", "gh", "delta", "rg", "duckdb", "tree-sitter",
      "ast-grep", "biome", "comrak", "marksman", "ruff",
      "shfmt", "sqruff", "stylua", "superhtml", "uv",
      "luajit", "luarocks", "luarocks-admin", "djot", "lunamark"
    }

    for _, name in ipairs(binaries) do
      local cmd = string.format(
        "find '%s' -path '*/%s/%s' -o -path '*/%s/bin/%s' | head -1",
        extract_dir, platform, name, platform, name
      )
      local binary_path = io.popen(cmd):read("*l")
      if binary_path and binary_path ~= "" then
        local target = bin_dir .. "/" .. name
        os.execute(string.format("ln -sf '%s' '%s'", binary_path, target))
        os.execute("chmod +x '" .. binary_path .. "'")
      end
    end
  end

  io.stderr:write("installation complete\n")
  io.stderr:write("add ~/.local/bin to PATH if needed\n")
end

local function cmd_list()
  local platform, err = detect_platform()
  if not platform then
    io.stderr:write("error: " .. err .. "\n")
    os.exit(1)
  end

  io.stdout:write("platform: " .. platform .. "\n")
  io.stdout:write("\nembedded binaries:\n")

  local binaries = {
    "tree-sitter", "rg", "delta", "nvim", "ruff", "sqruff",
    "superhtml", "uv", "gh", "duckdb", "stylua", "ast-grep",
    "biome", "marksman", "shfmt", "comrak",
    "luajit (+ luarocks, luarocks-admin, djot, lunamark)"
  }

  for _, name in ipairs(binaries) do
    io.stdout:write("  - " .. name .. "\n")
  end
end

local function cmd_version()
  io.stdout:write("home built COMMIT_PLACEHOLDER\n")
end

local function main(args)
  local cmd = args[1] or "help"

  if cmd == "unpack" then
    cmd_unpack()
  elseif cmd == "install" then
    cmd_install()
  elseif cmd == "list" then
    cmd_list()
  elseif cmd == "version" then
    cmd_version()
  else
    io.stderr:write("usage: home <command>\n")
    io.stderr:write("\ncommands:\n")
    io.stderr:write("  install  - install dotfiles and binaries to $HOME\n")
    io.stderr:write("  unpack   - output dotfiles.zip to stdout\n")
    io.stderr:write("  list     - list embedded binaries for current platform\n")
    io.stderr:write("  version  - show build version\n")
    os.exit(cmd == "help" and 0 or 1)
  end
end

main({...})
