#!/usr/bin/env cosmic
local fs = require("cosmic.fs")
local cio = require("cosmic.io")
local hash = require("cosmic.hash")
local env = require("cosmic.env")
local fv = require("fetch-verify")

local function test_fetch_and_verify_success()
  local tmpdir = env.get("TEST_TMPDIR")
  assert(tmpdir, "TEST_TMPDIR must be set")
  local path = fs.join(tmpdir, "test.txt")

  local url = "https://httpbin.org/robots.txt"
  -- fetch once to get the sha
  local fetch = require("cosmic.fetch")
  local result = fetch.Fetch(url)
  assert(result.ok, "pre-fetch failed")
  local sha = hash.sha256_hex(result.body)

  local ok, err = fv.fetch_and_verify(url, sha, path)
  assert(ok, "fetch_and_verify failed: " .. tostring(err))
  assert(fs.isfile(path), "expected file at " .. path)

  local content = cio.slurp(path)
  assert(content == result.body, "content mismatch")
end
test_fetch_and_verify_success()

local function test_fetch_and_verify_bad_sha()
  local tmpdir = env.get("TEST_TMPDIR")
  assert(tmpdir, "TEST_TMPDIR must be set")
  local path = fs.join(tmpdir, "bad_sha.txt")

  local ok, err = fv.fetch_and_verify(
    "https://httpbin.org/robots.txt",
    "0000000000000000000000000000000000000000000000000000000000000000",
    path
  )
  assert(not ok, "expected failure for bad sha")
  assert(err:find("sha256 mismatch"), "expected sha256 mismatch error, got: " .. err)
  assert(not fs.isfile(path), "file should not exist after sha mismatch")
end
test_fetch_and_verify_bad_sha()

local function test_fetch_and_verify_bad_url()
  local tmpdir = env.get("TEST_TMPDIR")
  assert(tmpdir, "TEST_TMPDIR must be set")
  local path = fs.join(tmpdir, "bad_url.txt")

  local ok, err = fv.fetch_and_verify(
    "https://httpbin.org/status/404",
    "0000000000000000000000000000000000000000000000000000000000000000",
    path
  )
  assert(not ok, "expected failure for 404")
  assert(err:find("fetch"), "expected fetch error, got: " .. tostring(err))
end
test_fetch_and_verify_bad_url()

local function test_fetch_and_verify_creates_dirs()
  local tmpdir = env.get("TEST_TMPDIR")
  assert(tmpdir, "TEST_TMPDIR must be set")
  local path = fs.join(tmpdir, "deep/nested/dir/file.txt")

  local url = "https://httpbin.org/robots.txt"
  local fetch = require("cosmic.fetch")
  local result = fetch.Fetch(url)
  assert(result.ok, "pre-fetch failed")
  local sha = hash.sha256_hex(result.body)

  local ok, err = fv.fetch_and_verify(url, sha, path)
  assert(ok, "fetch_and_verify failed: " .. tostring(err))
  assert(fs.isfile(path), "expected file at " .. path)
end
test_fetch_and_verify_creates_dirs()
