local lu = require("luaunit")
local unix = require("cosmo.unix")
local spawn = require("spawn").spawn
local path = require("cosmo.path")

local bin = path.join(os.getenv("TEST_BIN_DIR"), "bin", "marksman")

-- disable .NET diagnostics and redirect temp files to TEST_TMPDIR
local env = unix.environ()
env[#env + 1] = "DOTNET_EnableDiagnostics=0"
env[#env + 1] = "COMPlus_EnableDiagnostics=0"
env[#env + 1] = "TMPDIR=" .. TEST_TMPDIR
env[#env + 1] = "TMP=" .. TEST_TMPDIR
env[#env + 1] = "TEMP=" .. TEST_TMPDIR

TestMarksman = {}

function TestMarksman:test_version()
  local handle = spawn({ bin, "--version" }, { env = env })
  lu.assertEquals(handle:wait(), 0)
end
