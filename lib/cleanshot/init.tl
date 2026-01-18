local unix = require("cosmo.unix")
local cpath = require("cosmo.path")
local getopt = require("cosmo.getopt")

local VALID_COMMANDS: {string:boolean} = {
  ["all-in-one"] = true,
  ["capture-area"] = true,
  ["capture-fullscreen"] = true,
  ["capture-window"] = true,
  ["capture-previous-area"] = true,
  ["self-timer"] = true,
  ["scrolling-capture"] = true,
  ["record-screen"] = true,
  ["capture-text"] = true,
  ["pin"] = true,
  ["open-annotate"] = true,
  ["open-from-clipboard"] = true,
  ["toggle-desktop-icons"] = true,
  ["hide-desktop-icons"] = true,
  ["show-desktop-icons"] = true,
  ["add-quick-access-overlay"] = true,
  ["open-history"] = true,
  ["restore-recently-closed"] = true,
  ["open-settings"] = true,
}

local BOOLEAN_FLAGS: {string:boolean} = {
  start = true,
  autoscroll = true,
  linebreaks = true,
}

local CAPTURE_COMMANDS: {string:boolean} = {
  ["capture-area"] = true,
  ["capture-fullscreen"] = true,
  ["capture-window"] = true,
  ["capture-previous-area"] = true,
  ["self-timer"] = true,
  ["scrolling-capture"] = true,
}

local record Flags
  {string:string}
end

local function url_encode(str: string): string
  return str:gsub("([^%w%-%.%_%~])", function(c: string): string
    return string.format("%%%02X", string.byte(c))
  end)
end

local function expand_path(p: string): string
  if p:sub(1, 1) == "~" then
    local home = os.getenv("HOME")
    return home .. p:sub(2)
  end
  return p
end

local function parse_flags(args: {string}): Flags, string
  local longopts = {
    {"x", "required"},
    {"y", "required"},
    {"width", "required"},
    {"height", "required"},
    {"display", "required"},
    {"action", "required"},
    {"filepath", "required"},
    {"tab", "required"},
    {"path", "required"},
    {"start", "none"},
    {"autoscroll", "none"},
    {"linebreaks", "none"},
  }
  local parser = getopt.new(args, "", longopts)

  local flags: Flags = {}

  while true do
    local opt, optarg = parser:next()
    if not opt then break end

    if opt == "?" then
      return nil, "invalid option"
    elseif BOOLEAN_FLAGS[opt] then
      flags[opt] = "true"
    else
      flags[opt] = optarg
    end
  end

  return flags
end

local function build_url(subcommand: string, flags: Flags): string
  local url = "cleanshot://" .. subcommand
  local params: {string} = {}

  for key, value in pairs(flags as {string:string}) do
    if key ~= "path" then
      if key == "filepath" then
        table.insert(params, key .. "=" .. url_encode(value))
      else
        table.insert(params, key .. "=" .. value)
      end
    end
  end

  if #params > 0 then
    url = url .. "?" .. table.concat(params, "&")
  end

  return url
end

local function get_screenshot_dir(flags: Flags): string
  if (flags as {string:string}).path then
    return expand_path((flags as {string:string}).path)
  end
  return expand_path("~/Downloads/screens")
end

local function get_latest_file(dir: string): string, number
  local latest_time: number = 0
  local latest_file: string = nil

  for name in unix.opendir(dir) do
    if name ~= "." and name ~= ".." then
      local p = cpath.join(dir, name)
      local st = unix.stat(p)
      if st and unix.S_ISREG(st:mode()) then
        local mtime = st:mtim()
        if mtime > latest_time then
          latest_time = mtime
          latest_file = p
        end
      end
    end
  end

  return latest_file, latest_time
end

local function spawn(program: string, args: {string}): number, string
  local p = unix.commandv(program)
  if not p then
    return nil, "command not found: " .. program
  end

  local pid = unix.fork()
  if pid == 0 then
    unix.execve(p, args, unix.environ())
    unix.exit(1)
  elseif pid > 0 then
    local _, status = unix.wait()
    if unix.WIFEXITED(status) then
      return unix.WEXITSTATUS(status)
    end
    return 1
  else
    return nil, "fork failed"
  end
end

local function wait_for_new_screenshot(dir: string, baseline_time: number, timeout: number): string
  local start_time = os.time()

  while os.time() - start_time < timeout do
    local latest_file, latest_time = get_latest_file(dir)
    if latest_file and latest_time > baseline_time then
      return latest_file
    end
    unix.nanosleep(1)
  end

  return nil
end

local function should_wait_for_file(subcommand: string, flags: Flags): boolean
  local f = flags as {string:string}
  if not f.action or f.action ~= "save" then
    return false
  end
  return CAPTURE_COMMANDS[subcommand] == true
end

local function cmd_help(): number
  io.write([[usage: cleanshot <command> [flags]

commands:
  help                    Show this help
  latest                  Print path of most recent screenshot
  all-in-one              Launch all-in-one capture tool
  capture-area            Area capture
  capture-fullscreen      Fullscreen capture
  capture-window          Window capture
  capture-previous-area   Repeat last screenshot
  self-timer              Self-timer capture
  scrolling-capture       Scrolling capture
  record-screen           Screen recording
  capture-text            OCR text recognition
  pin                     Pin image file
  open-annotate           Annotate image
  open-from-clipboard     Annotate clipboard image
  toggle-desktop-icons    Toggle desktop icons visibility
  hide-desktop-icons      Hide desktop icons
  show-desktop-icons      Show desktop icons
  add-quick-access-overlay Add quick access overlay
  open-history            Open capture history
  restore-recently-closed Restore recently deleted
  open-settings           Open settings

flags:
  -x <int>               X coordinate
  -y <int>               Y coordinate
  -width <int>           Width in pixels
  -height <int>          Height in pixels
  -display <int>         Display number (1=main, 2=secondary, etc.)
  -action <string>       Post-capture action (copy, save, annotate, upload, pin)
  -filepath <path>       File path for pin/annotate
  -tab <string>          Settings tab name
  -start                 Auto-start (boolean)
  -autoscroll            Auto-scroll (boolean)
  -linebreaks            Preserve line breaks (boolean)
  -path <dir>            Screenshot directory (default: ~/Downloads/screens)

examples:
  cleanshot capture-area
  cleanshot capture-area -x 100 -y 120 -width 200 -height 150
  cleanshot capture-fullscreen -action save
  cleanshot scrolling-capture -start -autoscroll
  cleanshot record-screen -x 0 -y 0 -width 1920 -height 1080
  cleanshot capture-text -linebreaks
  cleanshot open-settings -tab shortcuts
]])
  return 0
end

local function cmd_latest(args: {string}): number
  local flags, err = parse_flags(args)
  if not flags then
    io.stderr:write("error: " .. err .. "\n")
    return 1
  end

  local dir = get_screenshot_dir(flags)
  local latest_file = get_latest_file(dir)

  if latest_file then
    io.write(latest_file .. "\n")
    return 0
  else
    io.stderr:write("error: no screenshots found in " .. dir .. "\n")
    return 1
  end
end

local function cmd_capture(subcommand: string, args: {string}): number
  local flags, err = parse_flags(args)
  if not flags then
    io.stderr:write("error: " .. err .. "\n")
    return 1
  end

  local baseline_time: number
  local dir: string

  if should_wait_for_file(subcommand, flags) then
    dir = get_screenshot_dir(flags)
    unix.makedirs(dir)
    local _: string
    _, baseline_time = get_latest_file(dir)
    baseline_time = baseline_time or 0
  end

  local url = build_url(subcommand, flags)
  local exit_code, spawn_err = spawn("open", {"open", url})
  if not exit_code then
    io.stderr:write("error: " .. spawn_err .. "\n")
    return 1
  end
  if exit_code ~= 0 then
    io.stderr:write("error: failed to execute cleanshot command\n")
    return 1
  end

  if baseline_time then
    local p = wait_for_new_screenshot(dir, baseline_time, 10)
    if p then
      io.write(p .. "\n")
    else
      io.stderr:write("warning: screenshot not detected within timeout\n")
    end
  end

  return 0
end

local function cmd_unknown(command: string): number
  io.stderr:write("unknown command: " .. command .. "\n")
  io.stderr:write("run 'cleanshot help' for usage\n")
  return 1
end

local function main(args: {string}): number
  if #args == 0 or args[1] == "help" or args[1] == "-h" or args[1] == "--help" then
    return cmd_help()
  end

  local command = args[1]
  local cmd_args = {table.unpack(args, 2)}

  if command == "latest" then
    return cmd_latest(cmd_args)
  end

  if VALID_COMMANDS[command] then
    return cmd_capture(command, cmd_args)
  end

  return cmd_unknown(command)
end

return {
  main = main,
  get_latest_file = get_latest_file,
  build_url = build_url,
  parse_flags = parse_flags,
}
