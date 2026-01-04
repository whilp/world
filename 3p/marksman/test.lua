local lu = require("luaunit")
local unix = require("cosmo.unix")
local spawn = require("cosmic.spawn")
local path = require("cosmo.path")
local env_helper = require("cosmic.env")

local bin = path.join(os.getenv("TEST_BIN_DIR"), "bin", "marksman")

-- disable .NET diagnostics and redirect temp files to TEST_TMPDIR
local env = unix.environ()
env_helper.set(env, "DOTNET_EnableDiagnostics", "0")
env_helper.set(env, "COMPlus_EnableDiagnostics", "0")
env_helper.set(env, "TMPDIR", TEST_TMPDIR)
env_helper.set(env, "TMP", TEST_TMPDIR)
env_helper.set(env, "TEMP", TEST_TMPDIR)

TestMarksman = {}

function TestMarksman:test_version()
  local handle = spawn({ bin, "--version" }, { env = env })
  lu.assertEquals(handle:wait(), 0)
end
