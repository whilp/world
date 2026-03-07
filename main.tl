local cio = require("cosmic.io")
local getopt = require("cosmic.getopt")
local proc = require("cosmic.proc")

local record Opts
  output: string
  list: boolean
  help: boolean
end

local TAR_PATH < const > = "/zip/embed/world.tar"

--- return the embedded tarball contents, or nil + error.
local function tar(path?: string): string, string
  path = path or TAR_PATH
  return cio.slurp(path)
end

--- return usage string.
local function usage(): string
  return "usage: world [options]\n"
  .. "  -o, --output <file>   write tarball to file (default: stdout)\n"
  .. "  -l, --list            list tarball contents\n"
  .. "  -h, --help            show this help\n"
  .. "\n"
  .. "examples:\n"
  .. "  world | tar xf - -C ~\n"
  .. "  world -o world.tar && tar xf world.tar -C ~\n"
  .. "  world --list\n"
end

--- parse command-line arguments into an Opts record.
local function parse_args(args: {string}): Opts
  local opts: Opts = {
    output = "",
    list = false,
    help = false,
  }

  local parser = getopt.new(args, "ho:l", {
      {name = "help", has_arg = "none", short = "h"},
      {name = "output", has_arg = "required", short = "o"},
      {name = "list", has_arg = "none", short = "l"},
    })

  while true do
    local opt, optarg = parser:next()
    if not opt then break end
    if opt == "h" then
      opts.help = true
    elseif opt == "o" then
      opts.output = optarg
    elseif opt == "l" then
      opts.list = true
    end
  end

  return opts
end

if proc.is_main() then
  local child = require("cosmic.child")

  local opts = parse_args(arg)

  if opts.help then
    io.stderr:write(usage())
    os.exit(0)
  end

  local content, err = tar()
  if not content then
    io.stderr:write("error: " .. (err or "no embedded tarball") .. "\n")
    os.exit(1)
  end

  if opts.list then
    local h = child.spawn({"tar", "tf", "-"}, {stdin = content})
    local ok, out, code = h:read()
    if ok then
      io.write(out)
    end
    os.exit(code as integer)
  end

  if opts.output ~= "" then
    local ok, write_err = cio.barf(opts.output, content)
    if not ok then
      io.stderr:write("error: " .. (write_err or "unknown") .. "\n")
      os.exit(1)
    end
  else
    io.write(content)
  end
end

return {
  tar = tar,
  usage = usage,
  parse_args = parse_args,
}
