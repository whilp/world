#!/usr/bin/env run-test.lua

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local json = require("cosmo").DecodeJson

global TEST_BIN: string
global TEST_TMPDIR: string

local bun = path.join(TEST_BIN, "bun")
local clasp = path.join(TEST_BIN, "clasp")
local srcdir = "lib/appscript"

local function test_gs_syntax()
  -- validate .gs files can be parsed as javascript
  local dir = unix.opendir(srcdir)
  assert(dir, "failed to open " .. srcdir)

  for entry in dir do
    if entry:match("%.gs$") then
      local filepath = path.join(srcdir, entry)
      local outdir = path.join(TEST_TMPDIR, "build")
      unix.makedirs(outdir)

      local ok, out = spawn({ bun, "build", filepath, "--no-bundle", "--outdir", outdir }):read()
      assert(ok, "bun build failed for " .. entry .. ": " .. tostring(out))
    end
  end
end

local function test_appsscript_json()
  -- validate appsscript.json is valid json
  local filepath = path.join(srcdir, "appsscript.json")
  local f = assert(io.open(filepath, "r"))
  local content = f:read("*a")
  f:close()

  local manifest = json(content)
  assert(manifest, "failed to parse appsscript.json")
  assert(manifest.timeZone, "appsscript.json missing timeZone")
  assert(manifest.dependencies, "appsscript.json missing dependencies")
  assert(manifest.dependencies.enabledAdvancedServices, "appsscript.json missing enabledAdvancedServices")
end

local function test_required_functions()
  -- check that key functions exist in the scripts
  local required: {string:{string}} = {
    ["colorize.gs"] = { "colorize", "setup", "isGroup", "getEventType" },
    ["auto-accept.gs"] = { "autoAcceptOtherCalendarInvites", "setupAutoAccept", "setOtherCalendar" },
    ["busy.gs"] = { "fillBusyEvents", "setupBusyFill" },
  }

  for filename, functions in pairs(required) do
    local filepath = path.join(srcdir, filename)
    local f = assert(io.open(filepath, "r"), "failed to open " .. filename)
    local content = f:read("*a")
    f:close()

    for _, fn in ipairs(functions) do
      local pattern = "function%s+" .. fn .. "%s*%("
      assert(content:match(pattern), filename .. " missing function: " .. fn)
    end
  end
end

local function test_clasp_help()
  -- verify clasp binary works
  local ok, out = spawn({ clasp, "--help" }):read()
  assert(ok, "clasp --help failed")
  assert((out as string):find("Apps Script", 1, true), "clasp should mention Apps Script")
end

test_gs_syntax()
test_appsscript_json()
test_required_functions()
test_clasp_help()
