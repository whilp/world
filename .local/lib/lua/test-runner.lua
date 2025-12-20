package.path = os.getenv("HOME") .. "/.local/lib/lua/?.lua;" .. package.path

lu = require('luaunit')

local function run_tests(test_dir)
  local cosmo = require('cosmo')
  local unix = cosmo.unix

  local dir = unix.opendir(test_dir)
  if not dir then
    io.stderr:write("error: cannot open test directory: " .. test_dir .. "\n")
    os.exit(1)
  end

  local test_files = {}
  while true do
    local name = dir:read()
    if not name then
      break
    end

    if name:match("^test_.*%.lua$") then
      table.insert(test_files, test_dir .. "/" .. name)
    end
  end
  dir:close()

  table.sort(test_files)

  if #test_files == 0 then
    io.stderr:write("no test files found in " .. test_dir .. "\n")
    os.exit(1)
  end

  for _, test_file in ipairs(test_files) do
    io.write("running " .. test_file .. "\n")
    dofile(test_file)
  end

  os.exit(lu.LuaUnit.run())
end

local test_dir = os.getenv("HOME") .. "/.local/lib/lua"
run_tests(test_dir)
