-- test_bootstrap.tl: tests for sprite bootstrap module

local cosmo = require("cosmo")
local path = require("cosmo.path")

local bootstrap = require("home.bootstrap")

--------------------------------------------------------------------------------
-- detect_platform tests
--------------------------------------------------------------------------------

local function test_detect_platform()
  local platform, err = bootstrap.detect_platform()
  assert(platform, "expected platform, got error: " .. tostring(err))

  -- platform should be os-arch format (arch may contain underscore like x86_64)
  assert(platform:match("^%w+%-[%w_]+$"), "expected os-arch format, got: " .. platform)

  -- should be one of the known platforms
  local known = {
    ["linux-x86_64"] = true,
    ["linux-arm64"] = true,
    ["darwin-arm64"] = true,
    ["darwin-x86_64"] = true,
  }
  assert(known[platform], "unexpected platform: " .. platform)

  io.stderr:write("detected platform: " .. platform .. "\n")
end
test_detect_platform()

--------------------------------------------------------------------------------
-- bootstrap binary tests (requires TEST_BIN)
--------------------------------------------------------------------------------

local TEST_BIN = os.getenv("TEST_BIN")
if TEST_BIN then
  local spawn = require("cosmic.spawn")
  local bootstrap_bin = path.join(TEST_BIN, "bootstrap")

  local function test_bootstrap_binary_exists()
    local st = require("cosmo.unix").stat(bootstrap_bin)
    assert(st, "bootstrap binary not found at " .. bootstrap_bin)
  end
  test_bootstrap_binary_exists()

  local function test_bootstrap_help()
    -- bootstrap with no network should fail gracefully
    -- just verify it runs and produces output
    local handle = spawn({bootstrap_bin})
    local exit_code = handle:wait()
    -- will fail because it can't fetch release, but that's expected
    assert(exit_code ~= nil, "expected exit code")
  end
  test_bootstrap_help()
end

print("all bootstrap tests passed")
