local cosmo = require("cosmo")
local unix = cosmo.unix

local function get_executable_path()
  local path

  -- Cosmopolitan sets arg[-1] to the actual executable path
  if arg and arg[-1] then
    path = arg[-1]
  -- Try arg[0]
  elseif arg and arg[0] then
    path = arg[0]
  -- Last resort: try /proc/self/exe
  else
    local f = io.open("/proc/self/exe", "r")
    if f then
      f:close()
      path = "/proc/self/exe"
    else
      error("cannot determine executable path")
    end
  end

  -- Always normalize to absolute path
  return unix.realpath(path)
end

local function mkdir_p(path)
  return cosmo.unix.makedirs(path)
end

local function copy_file(src, dst)
  local src_f = io.open(src, "rb")
  if not src_f then
    return false
  end

  local data = src_f:read("*a")
  src_f:close()

  local dst_f = io.open(dst, "wb")
  if not dst_f then
    return false
  end

  dst_f:write(data)
  dst_f:close()
  return true
end

local function cmd_unpack(dest)
  if not dest then
    io.stderr:write("error: destination path required\n")
    io.stderr:write("usage: home unpack <destination>\n")
    os.exit(1)
  end

  io.stderr:write("extracting to " .. dest .. "...\n")

  -- Create destination directory
  if not mkdir_p(dest) then
    io.stderr:write("error: failed to create destination directory\n")
    os.exit(1)
  end

  -- Recursively copy files from /zip/home/ to destination
  local function copy_from_zip(zip_path, dest_path, prefix)
    -- Try to open the path as a file first
    local f = io.open(zip_path, "rb")
    if f then
      f:close()
      -- It's a file, copy it
      if not copy_file(zip_path, dest_path) then
        io.stderr:write("warning: failed to copy " .. zip_path .. "\n")
      end
      return
    end

    -- It's a directory, list its contents using find via zip listing
    -- For now, we'll use a simpler approach: just copy known file paths
  end

  -- Use io.popen to get list of files from zip
  local handle = io.popen("unzip -Z1 '" .. get_executable_path() .. "' 2>/dev/null | grep '^home/'")
  if not handle then
    io.stderr:write("error: failed to list zip contents\n")
    os.exit(1)
  end

  local files = {}
  for line in handle:lines() do
    table.insert(files, line)
  end
  handle:close()

  -- Copy each file from /zip/ to destination
  for _, file_path in ipairs(files) do
    -- file_path is like "home/.zshrc"
    local zip_file_path = "/zip/" .. file_path
    local dest_file_path = dest .. "/" .. file_path:sub(6) -- Remove "home/" prefix

    -- Check if it's a directory (ends with /)
    if not file_path:match("/$") then
      -- Create parent directory
      local parent_dir = cosmo.path.dirname(dest_file_path)
      mkdir_p(parent_dir)

      -- Copy file
      if not copy_file(zip_file_path, dest_file_path) then
        io.stderr:write("warning: failed to copy " .. file_path .. "\n")
      end
    else
      -- Create directory
      mkdir_p(dest .. "/" .. file_path:sub(6))
    end
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
    "nvim",
    "gh",
    "delta",
    "rg",
    "duckdb",
    "tree-sitter",
    "ast-grep",
    "biome",
    "comrak",
    "marksman",
    "ruff",
    "shfmt",
    "sqruff",
    "stylua",
    "superhtml",
    "uv",
    "luajit",
    "luarocks",
    "luarocks-admin",
    "djot",
    "lunamark",
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
    io.stderr:write("  unpack <dest>  - extract dotfiles and binaries to destination\n")
    io.stderr:write("  list           - list embedded tools\n")
    io.stderr:write("  version        - show build version\n")
    os.exit(cmd == "help" and 0 or 1)
  end
end

main({ ... })
