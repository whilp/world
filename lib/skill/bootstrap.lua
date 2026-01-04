-- bootstrap - setup environment for Claude Code web

local unix = require("cosmo.unix")

local function main()
  local env_file = os.getenv("CLAUDE_ENV_FILE")
  if not env_file then
    return 0
  end

  local cwd = unix.getcwd()
  if not cwd then
    return 1, "bootstrap: failed to get current working directory"
  end

  local bin_path = cwd .. "/bin"
  local export_line = string.format('export PATH="%s:$PATH"\n', bin_path)

  local f, err = io.open(env_file, "a")
  if not f then
    return 1, string.format("bootstrap: failed to open %s: %s", env_file, err)
  end

  local ok, write_err = f:write(export_line)
  f:close()

  if not ok then
    return 1, string.format("bootstrap: failed to write to %s: %s", env_file, write_err)
  end

  return 0
end

return {
  main = main,
  _VERSION = "0.1.0",
  _DESCRIPTION = "Bootstrap environment for Claude Code web",
}
