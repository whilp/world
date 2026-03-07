local fs = require("cosmic.fs")
local cio = require("cosmic.io")
local proc = require("cosmic.proc")

local record ExtractResult
  rel: string
  target: string
end

local EMBED_ROOT < const > = "/zip/embed/"

--- list embedded files under root, returns relative paths.
local function list(root?: string): {string}
  root = root or EMBED_ROOT
  local files = fs.collect(root, "*")
  table.sort(files)
  local result: {string} = {}
  for _, f in ipairs(files) do
    table.insert(result, f:sub(#root + 1))
  end
  return result
end

--- extract embedded files to dest. returns list of extracted files.
local function extract(dest: string, root?: string): {ExtractResult}, string
  root = root or EMBED_ROOT
  local files = fs.collect(root, "*")
  table.sort(files)

  if #files == 0 then
    return nil, "no embedded files found"
  end

  local results: {ExtractResult} = {}

  for _, f in ipairs(files) do
    local rel = f:sub(#root + 1)
    local target = fs.join(dest, rel)

    local target_dir = fs.dirname(target)
    local ok, err = fs.makedirs(target_dir)
    if not ok then
      return nil, "mkdir " .. target_dir .. ": " .. (err or "unknown")
    end

    local content, read_err = cio.slurp(f)
    if not content then
      return nil, "read " .. f .. ": " .. (read_err or "unknown")
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
      return nil, "write " .. target .. ": " .. (write_err or "unknown")
    end

    table.insert(results, {rel = rel, target = target})
  end

  return results
end

--- return usage string.
local function usage(): string
  return "usage: world [--dest <dir>] [--list]\n"
  .. "  --dest <dir>   destination directory (default: $HOME)\n"
  .. "  --list         list embedded files\n"
  .. "  --help         show this help\n"
end

if proc.is_main() then
  local getopt = require("cosmic.getopt")

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
      io.stderr:write(usage())
      os.exit(0)
    elseif opt == "d" then
      dest = optarg
    elseif opt == "l" then
      list_only = true
    end
  end

  if list_only then
    for _, rel in ipairs(list()) do
      print(rel)
    end
    os.exit(0)
  end

  local results, err = extract(dest)
  if not results then
    io.stderr:write("error: " .. (err or "unknown") .. "\n")
    os.exit(1)
  end

  for _, r in ipairs(results) do
    print(r.rel .. " -> " .. r.target)
  end
end

return {
  list = list,
  extract = extract,
  usage = usage,
}
