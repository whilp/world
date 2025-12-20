local cosmo = require("cosmo")
local unix = cosmo.unix

-- Atomic file copy with permissions
-- Creates file with restrictive perms, writes data, then sets final mode
-- Returns ok, err where ok is true on success, false on failure
local function copy_file(src, dst, mode, overwrite)
  -- Read source
  local src_f, err = io.open(src, "rb")
  if not src_f then
    return false, "failed to open source: " .. (err or "unknown error")
  end
  local data = src_f:read("*a")
  src_f:close()

  -- Create or overwrite destination with restrictive permissions
  local flags = unix.O_WRONLY | unix.O_CREAT
  if overwrite then
    flags = flags | unix.O_TRUNC
  else
    flags = flags | unix.O_EXCL
  end

  local fd = unix.open(dst, flags, 0600)
  if not fd or fd < 0 then
    if overwrite then
      return false, "failed to open destination for writing"
    else
      return false, "destination already exists (use --force to overwrite)"
    end
  end

  -- Write data
  local bytes_written = unix.write(fd, data)
  local success = bytes_written == #data

  if not success then
    unix.close(fd)
    return false, "failed to write data (wrote " .. bytes_written .. " of " .. #data .. " bytes)"
  end

  -- Set final permissions if specified
  if mode then
    local chmod_ok = unix.chmod(dst, mode)
    if not chmod_ok then
      unix.close(fd)
      return false, "failed to set permissions"
    end
  end

  unix.close(fd)
  return true
end

local function cmd_unpack(dest, force)
  if not dest then
    io.stderr:write("error: destination path required\n")
    io.stderr:write("usage: home unpack [--force] <destination>\n")
    os.exit(1)
  end

  io.stderr:write("extracting to " .. dest .. "...\n")
  if force then
    io.stderr:write("overwrite mode enabled\n")
  end

  -- Create destination directory
  if not unix.makedirs(dest) then
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

  -- Read manifest to get list of files with modes
  local manifest = io.open("/zip/MANIFEST.txt", "r")
  if not manifest then
    io.stderr:write("error: failed to read manifest\n")
    os.exit(1)
  end

  local files = {}
  for line in manifest:lines() do
    -- Skip comments and empty lines
    if not line:match("^%s*#") and line:match("%S") then
      -- Parse "filepath mode" format
      local file_path, mode_str = line:match("^(.-)%s+(%x+)$")
      if file_path and mode_str then
        local mode = tonumber(mode_str, 8) -- Parse octal mode
        table.insert(files, {path = file_path, mode = mode})
      else
        -- Fallback for old format (no mode)
        table.insert(files, {path = line, mode = nil})
      end
    end
  end
  manifest:close()

  -- Copy each file from /zip/ to destination
  for _, file_info in ipairs(files) do
    local file_path = file_info.path
    local mode = file_info.mode
    -- file_path is like "home/.zshrc"
    local zip_file_path = "/zip/" .. file_path
    local dest_file_path = dest .. "/" .. file_path:sub(6) -- Remove "home/" prefix

    -- Check if it's a directory (ends with /)
    if not file_path:match("/$") then
      -- Create parent directory
      local parent_dir = cosmo.path.dirname(dest_file_path)
      unix.makedirs(parent_dir)

      -- Copy file atomically with permissions
      local ok, err = copy_file(zip_file_path, dest_file_path, mode, force)
      if not ok then
        io.stderr:write("warning: failed to copy " .. file_path .. ": " .. (err or "unknown error") .. "\n")
      end
    else
      -- Create directory
      unix.makedirs(dest .. "/" .. file_path:sub(6))
    end
  end

  io.stderr:write("extraction complete\n")
  io.stderr:write("add " .. dest .. "/.local/bin to PATH if needed\n")
end

local function cmd_list()
  -- Read manifest to get list of files
  local manifest = io.open("/zip/MANIFEST.txt", "r")
  if not manifest then
    io.stderr:write("error: failed to read manifest\n")
    os.exit(1)
  end

  -- Count files and collect unique tools from .local/bin
  local file_count = 0
  local tools = {}
  for line in manifest:lines() do
    if not line:match("^%s*#") and line:match("%S") then
      file_count = file_count + 1
      -- Parse "filepath mode" format, extract filepath
      local file_path = line:match("^(.-)%s+%x+$") or line
      -- Extract tool name from .local/bin paths
      local tool = file_path:match("home/%.local/bin/([^/]+)$")
      if tool and not tools[tool] then
        tools[tool] = true
      end
    end
  end
  manifest:close()

  io.stdout:write("embedded files: " .. file_count .. " total\n")
  io.stdout:write("  - dotfiles (~/.zshrc, ~/.config/*, etc.)\n")
  io.stdout:write("  - binaries (~/.local/bin/*)\n")
  io.stdout:write("\nembedded tools:\n")

  -- Sort tools alphabetically
  local sorted_tools = {}
  for tool, _ in pairs(tools) do
    table.insert(sorted_tools, tool)
  end
  table.sort(sorted_tools)

  for _, tool in ipairs(sorted_tools) do
    io.stdout:write("  - " .. tool .. "\n")
  end
end

local function cmd_version()
  io.stdout:write("home built COMMIT_PLACEHOLDER\n")
end

local function main(args)
  local cmd = args[1] or "help"

  if cmd == "unpack" then
    -- Parse flags and arguments
    local force = false
    local dest
    local i = 2
    while i <= #args do
      if args[i] == "--force" or args[i] == "-f" then
        force = true
      elseif not dest then
        dest = args[i]
      end
      i = i + 1
    end
    cmd_unpack(dest, force)
  elseif cmd == "list" then
    cmd_list()
  elseif cmd == "version" then
    cmd_version()
  else
    io.stderr:write("usage: home <command> [args]\n")
    io.stderr:write("\ncommands:\n")
    io.stderr:write("  unpack [--force] <dest>  - extract dotfiles and binaries to destination\n")
    io.stderr:write("  list                     - list embedded tools\n")
    io.stderr:write("  version                  - show build version\n")
    io.stderr:write("\noptions:\n")
    io.stderr:write("  --force, -f              - overwrite existing files\n")
    os.exit(cmd == "help" and 0 or 1)
  end
end

main({ ... })
