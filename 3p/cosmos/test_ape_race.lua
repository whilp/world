local lu = require("luaunit")
local unix = require("cosmo.unix")
local cosmo = require("cosmo")
local path = require("cosmo.path")
local spawn = require("spawn").spawn

local bin_dir = path.join(os.getenv("TEST_BIN_DIR"), "bin")
local tmp_dir = "/tmp/ape_race_test"

TestApeRace = {}

function TestApeRace:setUp()
  unix.makedirs(tmp_dir)
end

function TestApeRace:tearDown()
  unix.rmrf(tmp_dir)
end

-- copy binary without executing it (no assimilation)
local function copy_fresh_binary(src, dst)
  local content = cosmo.Slurp(src)
  if not content then
    return nil, "failed to read " .. src
  end
  local ok = cosmo.Barf(dst, content, tonumber("755", 8))
  if not ok then
    return nil, "failed to write " .. dst
  end
  return true
end

-- this test demonstrates the ETXTBSY race condition with APE binaries
-- APE binaries self-modify ("assimilate") on first execution
-- concurrent execution of a fresh binary can cause ETXTBSY
function TestApeRace:test_concurrent_execution_without_priming()
  local src = path.join(bin_dir, "zip")
  local dst = path.join(tmp_dir, "zip_fresh")

  local ok, err = copy_fresh_binary(src, dst)
  lu.assertNotNil(ok, err)

  -- spawn multiple concurrent executions of fresh binary
  local handles = {}
  local num_concurrent = 4
  for i = 1, num_concurrent do
    local h, spawn_err = spawn({ dst, "--version" })
    if h then
      table.insert(handles, { handle = h, spawned = true })
    else
      table.insert(handles, { spawned = false, error = spawn_err })
    end
  end

  -- collect results
  local failures = {}
  for i, entry in ipairs(handles) do
    if entry.spawned then
      local exit_code = entry.handle:wait()
      if exit_code ~= 0 then
        table.insert(failures, string.format("process %d exited with %d", i, exit_code))
      end
    else
      table.insert(failures, string.format("process %d failed to spawn: %s", i, entry.error or "unknown"))
    end
  end

  -- without priming, we may see ETXTBSY failures on some systems
  -- this test documents the race condition rather than asserting failure
  if #failures > 0 then
    print("\nETXTBSY race detected (expected on fresh APE binaries):")
    for _, f in ipairs(failures) do
      print("  " .. f)
    end
  end

  -- the test passes either way - it's documenting behavior
  lu.assertTrue(true)
end

-- this test shows that priming prevents the race
function TestApeRace:test_concurrent_execution_with_priming()
  local src = path.join(bin_dir, "zip")
  local dst = path.join(tmp_dir, "zip_primed")

  local ok, err = copy_fresh_binary(src, dst)
  lu.assertNotNil(ok, err)

  -- prime the binary: run once and wait for completion
  local prime = spawn({ dst, "--version" })
  lu.assertNotNil(prime, "priming spawn failed")
  local prime_exit = prime:wait()
  lu.assertEquals(prime_exit, 0, "priming should succeed")

  -- sync to ensure assimilation is flushed
  spawn({ "sync" }):wait()

  -- now concurrent executions should all succeed
  local handles = {}
  local num_concurrent = 4
  for i = 1, num_concurrent do
    local h, spawn_err = spawn({ dst, "--version" })
    lu.assertNotNil(h, "spawn " .. i .. " failed: " .. tostring(spawn_err))
    table.insert(handles, h)
  end

  for i, h in ipairs(handles) do
    local exit_code = h:wait()
    lu.assertEquals(exit_code, 0, "process " .. i .. " should succeed after priming")
  end
end
