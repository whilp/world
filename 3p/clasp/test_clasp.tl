#!/usr/bin/env run-test.lua

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

local test_bin = os.getenv("TEST_BIN") or ""
local bin = path.join(test_bin, "clasp")

local function test_script_exists()
  local cosmo = require("cosmo")
  local content = cosmo.Slurp(bin)
  assert(content, "clasp script not found at " .. bin)
  assert(content:find("bun", 1, true), "clasp script should reference bun")
  assert(content:find("src/index.ts", 1, true), "clasp script should run src/index.ts")
end
test_script_exists()

global TEST_DIR: string

local function test_source_exists()
  local cosmo = require("cosmo")
  local index = path.join(TEST_DIR, "src", "index.ts")
  local content = cosmo.Slurp(index)
  assert(content, "clasp source not found at " .. index)
  assert(content:find("clasp", 1, true), "expected clasp in source")
end
test_source_exists()

local function test_package_json()
  local cosmo = require("cosmo")
  local pkg = path.join(TEST_DIR, "package.json")
  local content = cosmo.Slurp(pkg)
  assert(content, "package.json not found")
  local ok, data = pcall(cosmo.DecodeJson, content)
  assert(ok and data, "package.json should be valid JSON")
  assert(data.name == "@google/clasp", "expected @google/clasp package")
end
test_package_json()
