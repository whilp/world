local cosmo = require("cosmo")
local unix = cosmo.unix

-- Atomic file copy with permissions
-- Creates file with restrictive perms, writes data, then sets final mode
local function copy_file(src, dst, mode)
  -- Read source
  local src_f = io.open(src, "rb")
  if not src_f then
    return false
  end
  local data = src_f:read("*a")
  src_f:close()

  -- Create destination atomically with restrictive permissions
  local fd = unix.open(dst, unix.O_WRONLY | unix.O_CREAT | unix.O_EXCL, 0600)
  if not fd or fd < 0 then
    return false
  end

  -- Write data
  local bytes_written = unix.write(fd, data)
  local success = bytes_written == #data

  -- Set final permissions if specified and write succeeded
  if mode and success then
    unix.chmod(dst, mode)
  end

  unix.close(fd)
  return success
end

local function cmd_unpack(dest)
  if not dest then
    io.stderr:write("error: destination path required\n")
    io.stderr:write("usage: home unpack <destination>\n")
    os.exit(1)
  end

  io.stderr:write("extracting to " .. dest .. "...\n")

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
      if not copy_file(zip_file_path, dest_file_path, mode) then
        io.stderr:write("warning: failed to copy " .. file_path .. "\n")
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
