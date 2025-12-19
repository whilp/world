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

local function cmd_version()
  io.stdout:write("home built COMMIT_PLACEHOLDER\n")
end

local function main(args)
  local cmd = args[1] or "help"

  if cmd == "unpack" then
    cmd_unpack()
  elseif cmd == "version" then
    cmd_version()
  else
    io.stderr:write("usage: home unpack > dotfiles.zip\n")
    io.stderr:write("       unzip dotfiles.zip -d ~/\n")
    io.stderr:write("       home version\n")
    os.exit(cmd == "help" and 0 or 1)
  end
end

main({...})
