local fs = require("cosmic.fs")
local cio = require("cosmic.io")
local getopt = require("cosmic.getopt")

local function usage()
  io.stderr:write("usage: world [--dest <dir>] [--list]\n")
  io.stderr:write("  --dest <dir>   destination directory (default: $HOME)\n")
  io.stderr:write("  --list         list embedded files\n")
  io.stderr:write("  --help         show this help\n")
end

local parser = getopt.new(arg, "hd:l", {
    {name = "help", has_arg = "none", short = "h"},
    {name = "dest", has_arg = "required", short = "d"},
    {name = "list", has_arg = "none", short = "l"},
  })

local dest: string = os.getenv("HOME") or "."
local list_only = false

while true do
  local opt, optarg = parser:next()
  if not opt then break end
  if opt == "h" then
    usage()
    os.exit(0)
  elseif opt == "d" then
    dest = optarg
  elseif opt == "l" then
    list_only = true
  end
end

local embed_root = "/zip/embed/"

--- collect all embedded files under /zip/embed/
local files = fs.collect(embed_root, "*")
table.sort(files)

if list_only then
  for _, f in ipairs(files) do
    local rel = f:sub(#embed_root + 1)
    print(rel)
  end
  os.exit(0)
end

if #files == 0 then
  io.stderr:write("no embedded files found\n")
  os.exit(1)
end

--- extract files to dest
for _, f in ipairs(files) do
  local rel = f:sub(#embed_root + 1)
  local target = fs.join(dest, rel)

  local target_dir = fs.dirname(target)
  local ok, err = fs.makedirs(target_dir)
  if not ok then
    io.stderr:write("error: mkdir " .. target_dir .. ": " .. (err or "unknown") .. "\n")
    os.exit(1)
  end

  local content, read_err = cio.slurp(f)
  if not content then
    io.stderr:write("error: read " .. f .. ": " .. (read_err or "unknown") .. "\n")
    os.exit(1)
  end

  --- preserve executable bit from embedded file
  local st = fs.stat(f)
  local mode = 0x1A4 -- 0644
  if st then
    local m = st:mode()
    if m & 0x49 ~= 0 then -- 0111 = any execute bit
      mode = 0x1ED -- 0755
    end
  end

  local write_ok, write_err = cio.barf(target, content, mode)
  if not write_ok then
    io.stderr:write("error: write " .. target .. ": " .. (write_err or "unknown") .. "\n")
    os.exit(1)
  end

  print(rel .. " -> " .. target)
end
