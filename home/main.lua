local function get_executable_path()
  -- Cosmopolitan sets arg[-1] to the actual executable path
  if arg and arg[-1] then
    return arg[-1]
  end

  -- Try arg[0]
  if arg and arg[0] then
    return arg[0]
  end

  -- Last resort: try /proc/self/exe (though unzip may not work with symlinks)
  local f = io.open("/proc/self/exe", "r")
  if f then
    f:close()
    return "/proc/self/exe"
  end

  error("cannot determine executable path")
end

local function cmd_unpack(dest)
  dest = dest or os.getenv("HOME")
  if not dest or dest == "" then
    io.stderr:write("error: destination not specified and HOME not set\n")
    os.exit(1)
  end

  local exe_path = get_executable_path()
  io.stderr:write("extracting to " .. dest .. "...\n")

  local cmd = string.format("unzip -q -o '%s' -d '%s'", exe_path, dest)
  local ret = os.execute(cmd)
  if ret ~= 0 and ret ~= true then
    io.stderr:write("error: extraction failed\n")
    os.exit(1)
  end

  io.stderr:write("extraction complete\n")
  io.stderr:write("add " .. dest .. "/.local/bin to PATH if needed\n")
end

local function cmd_list()
  io.stdout:write("embedded files:\n")
  io.stdout:write("  - dotfiles (~/.zshrc, ~/.config/*, etc.)\n")
  io.stdout:write("  - binaries (~/.local/bin/* -> ~/.local/share/home-binaries/*)\n")
  io.stdout:write("\nembedded tools:\n")

  local tools = {
    "nvim", "gh", "delta", "rg", "duckdb", "tree-sitter",
    "ast-grep", "biome", "comrak", "marksman", "ruff",
    "shfmt", "sqruff", "stylua", "superhtml", "uv",
    "luajit", "luarocks", "luarocks-admin", "djot", "lunamark"
  }

  for _, tool in ipairs(tools) do
    io.stdout:write("  - " .. tool .. "\n")
  end
end

local function cmd_version()
  io.stdout:write("home built COMMIT_PLACEHOLDER\n")
end

local function main(args)
  local cmd = args[1] or "help"

  if cmd == "unpack" then
    cmd_unpack(args[2])
  elseif cmd == "list" then
    cmd_list()
  elseif cmd == "version" then
    cmd_version()
  else
    io.stderr:write("usage: home <command> [args]\n")
    io.stderr:write("\ncommands:\n")
    io.stderr:write("  unpack [dest]  - extract dotfiles and binaries (default: $HOME)\n")
    io.stderr:write("  list           - list embedded tools\n")
    io.stderr:write("  version        - show build version\n")
    os.exit(cmd == "help" and 0 or 1)
  end
end

main({...})
