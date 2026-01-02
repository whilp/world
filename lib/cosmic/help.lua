-- cosmic.help: show available modules and usage

local modules = {
  { name = "cosmic.spawn", desc = "process spawning with stdin/stdout/stderr capture" },
  { name = "cosmic.walk", desc = "directory tree walking and file collection" },
}

local function print_help()
  print("cosmic-lua: cosmopolitan lua with batteries included")
  print("")
  print("available modules:")
  for _, m in ipairs(modules) do
    print(string.format("  %-20s %s", m.name, m.desc))
  end
  print("")
  print("bundled 3p libraries:")
  print("  luaunit              unit testing framework")
  print("  argparse             command-line argument parsing")
  print("  lfs                  lua file system operations")
  print("")
  print("built-in cosmo modules (from cosmopolitan):")
  print("  cosmo                top-level (Fetch, EncodeJson, etc)")
  print("  cosmo.unix           unix syscalls (fork, exec, pipe, etc)")
  print("  cosmo.path           path manipulation (join, basename, etc)")
  print("")
  print("example:")
  print("  local spawn = require('cosmic.spawn')")
  print("  local ok, out = spawn({'ls', '-la'}):read()")
end

print_help()

return {
  modules = modules,
  print_help = print_help,
}
