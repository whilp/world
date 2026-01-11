#!/usr/bin/env run-test.lua
-- ast-grep ignore: test file

local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")
local version = require("version")

local TEST_TMPDIR = os.getenv("TEST_TMPDIR") or "/tmp"

local record SpawnHandle
  read: function(self: SpawnHandle): boolean, string
  wait: function(self: SpawnHandle): integer
end

local record VersionInfo
  version: string
  sha: string
  path: string
  dir: string
end

local record StatResult
  mode: function(self: StatResult): integer
end

local home_bin = path.join(os.getenv("TEST_BIN") or "", "home")

-- Test 1: Built binary contains versioned directories for 3p tools
local ok, list_output = (spawn({home_bin, "list"}) as SpawnHandle):read()
assert(ok, "home list failed")

-- Check gh has versioned directory
local gh_versioned = list_output:match("%.local/share/gh/([%d%.%-]+%-%x+)/")
assert(gh_versioned, "gh should have versioned directory in home binary")
assert(gh_versioned:match("^%d+%.%d+%.%d+%-%x+$"), "gh version format: " .. gh_versioned)

-- Check nvim has versioned directory
local nvim_versioned = list_output:match("%.local/share/nvim/([^/]+)/")
assert(nvim_versioned, "nvim should have versioned directory in home binary")
assert(nvim_versioned:match("%-%x+$"), "nvim has sha suffix: " .. nvim_versioned)

-- Check delta has versioned directory
local delta_versioned = list_output:match("%.local/share/delta/([%d%.%-]+%-%x+)/")
assert(delta_versioned, "delta should have versioned directory in home binary")

-- Test 2: Unpack preserves versioned directory structure
local test_home = path.join(TEST_TMPDIR, "versioned_test")
unix.makedirs(test_home)

ok = (spawn({home_bin, "unpack", test_home}) as SpawnHandle):wait() == 0
assert(ok, "home unpack failed")

-- Verify gh versioned directory exists
local gh_share = path.join(test_home, ".local", "share", "gh")
local gh_dir = unix.opendir(gh_share)
assert(gh_dir, "gh share directory should exist")

local gh_versions: {string} = {}
for entry in gh_dir do
  if entry ~= "." and entry ~= ".." and version.is_version_dir(entry) then
    table.insert(gh_versions, entry)
  end
end
assert(#gh_versions == 1, "should have exactly one gh version, got " .. #gh_versions)
assert(gh_versions[1]:match("^%d+%.%d+%.%d+%-%x+$"), "gh version dir format: " .. gh_versions[1])

-- Verify gh binary exists in versioned directory
local gh_bin = path.join(gh_share, gh_versions[1], "bin", "gh")
local gh_stat = unix.stat(gh_bin) as StatResult
assert(gh_stat, "gh binary should exist at: " .. gh_bin)
assert(not unix.S_ISDIR(gh_stat:mode()), "gh binary should be a file")

-- Verify nvim versioned directory exists
local nvim_share = path.join(test_home, ".local", "share", "nvim")
local nvim_dir_obj = unix.opendir(nvim_share)
assert(nvim_dir_obj, "nvim share directory should exist")

local nvim_versions: {string} = {}
for entry in nvim_dir_obj do
  if entry ~= "." and entry ~= ".." and version.is_version_dir(entry) then
    table.insert(nvim_versions, entry)
  end
end
assert(#nvim_versions == 1, "should have exactly one nvim version, got " .. #nvim_versions)
assert(nvim_versions[1]:match("%-%x+$"), "nvim version has sha: " .. nvim_versions[1])

-- Verify nvim binary exists in versioned directory
local nvim_bin = path.join(nvim_share, nvim_versions[1], "bin", "nvim")
local nvim_stat = unix.stat(nvim_bin) as StatResult
assert(nvim_stat, "nvim binary should exist at: " .. nvim_bin)
assert(not unix.S_ISDIR(nvim_stat:mode()), "nvim binary should be a file")

-- Verify symlinks to versioned directories
local gh_bin_link = path.join(gh_share, "bin")
local gh_link_stat = unix.stat(gh_bin_link, unix.AT_SYMLINK_NOFOLLOW) as StatResult
assert(gh_link_stat, "gh bin symlink should exist")
assert(unix.S_ISLNK(gh_link_stat:mode()), "gh bin should be a symlink")
local gh_link_target = unix.readlink(gh_bin_link, 1024)
assert(gh_link_target == gh_versions[1] .. "/bin", "gh bin symlink target: " .. gh_link_target)

local nvim_bin_link = path.join(nvim_share, "bin")
local nvim_link_stat = unix.stat(nvim_bin_link, unix.AT_SYMLINK_NOFOLLOW) as StatResult
assert(nvim_link_stat, "nvim bin symlink should exist")
assert(unix.S_ISLNK(nvim_link_stat:mode()), "nvim bin should be a symlink")
local nvim_link_target = unix.readlink(nvim_bin_link, 1024)
assert(nvim_link_target == nvim_versions[1] .. "/bin", "nvim bin symlink target: " .. nvim_link_target)

local nvim_share_link = path.join(nvim_share, "share")
local nvim_share_stat = unix.stat(nvim_share_link, unix.AT_SYMLINK_NOFOLLOW) as StatResult
assert(nvim_share_stat, "nvim share symlink should exist")
assert(unix.S_ISLNK(nvim_share_stat:mode()), "nvim share should be a symlink")

-- Test 3: version.find_latest() works with unpacked structure
local nvim_latest = version.find_latest("nvim", path.join(test_home, ".local", "share")) as VersionInfo
assert(nvim_latest, "version.find_latest should find nvim")
assert(nvim_latest.version, "nvim latest should have version")
assert(nvim_latest.sha, "nvim latest should have sha")
assert(nvim_latest.path, "nvim latest should have path")
assert(nvim_latest.dir, "nvim latest should have dir")
assert(nvim_latest.path == nvim_bin, "nvim path should match: " .. nvim_latest.path .. " vs " .. nvim_bin)

-- Test 4: version.scan_versions() returns versioned info
local nvim_all = version.scan_versions("nvim", path.join(test_home, ".local", "share")) as {VersionInfo}
assert(#nvim_all == 1, "should find one nvim version, got " .. #nvim_all)
assert(nvim_all[1].version, "scanned version has version field")
assert(nvim_all[1].sha, "scanned version has sha field")
assert(nvim_all[1].path, "scanned version has path field")
assert(nvim_all[1].dir, "scanned version has dir field")

-- Test 5: Verify all bundled tools have versioned directories
local tools: {string} = {
  "ast-grep", "biome", "comrak", "delta", "duckdb", "gh",
  "luacheck", "marksman", "rg", "ruff", "shfmt", "sqruff",
  "stylua", "superhtml", "tree-sitter", "uv"
}

for _, tool in ipairs(tools) do
  local tool_share = path.join(test_home, ".local", "share", tool)
  local tool_dir = unix.opendir(tool_share)
  assert(tool_dir, tool .. " share directory should exist")

  local tool_versions: {string} = {}
  for entry in tool_dir do
    if entry ~= "." and entry ~= ".." and version.is_version_dir(entry) then
      table.insert(tool_versions, entry)
    end
  end

  assert(#tool_versions == 1, tool .. " should have exactly one version, got " .. #tool_versions)
  assert(tool_versions[1]:match("%-%x+$"), tool .. " version has sha: " .. tool_versions[1])

  -- Verify symlinks exist for items in versioned directory
  local versioned_path = path.join(tool_share, tool_versions[1])
  local versioned_dir = unix.opendir(versioned_path)
  assert(versioned_dir, tool .. " versioned directory should be openable")

  -- Check if bin directory exists in versioned dir
  local has_bin = false
  for item in versioned_dir do
    if item == "bin" then
      local item_path = path.join(versioned_path, item)
      local item_stat = unix.stat(item_path) as StatResult
      if item_stat and unix.S_ISDIR(item_stat:mode()) then
        has_bin = true
      end
    end
  end

  -- If versioned dir has bin/, verify bin symlink exists at tool root
  if has_bin then
    local bin_link = path.join(tool_share, "bin")
    local bin_stat = unix.stat(bin_link, unix.AT_SYMLINK_NOFOLLOW) as StatResult
    assert(bin_stat, tool .. " bin symlink should exist")
    assert(unix.S_ISLNK(bin_stat:mode()), tool .. " bin should be a symlink")
    local bin_target = unix.readlink(bin_link, 1024)
    assert(bin_target == tool_versions[1] .. "/bin", tool .. " bin symlink target: " .. bin_target)
  end
end

-- Test 6: Multiple versions side-by-side (simulate)
local multi_home = path.join(TEST_TMPDIR, "multi_version_test")
unix.makedirs(path.join(multi_home, ".local", "share", "test-tool"))

-- Create two fake versions
local v1_dir = path.join(multi_home, ".local", "share", "test-tool", "1.0.0-abc123")
local v2_dir = path.join(multi_home, ".local", "share", "test-tool", "2.0.0-def456")
unix.makedirs(path.join(v1_dir, "bin"))
unix.makedirs(path.join(v2_dir, "bin"))

-- Create fake binaries
local v1_bin = path.join(v1_dir, "bin", "test-tool")
local v2_bin = path.join(v2_dir, "bin", "test-tool")
local fd = unix.open(v1_bin, unix.O_WRONLY | unix.O_CREAT, tonumber("755", 8))
unix.close(fd)
fd = unix.open(v2_bin, unix.O_WRONLY | unix.O_CREAT, tonumber("755", 8))
unix.close(fd)

-- Scan should find both versions
local all_versions = version.scan_versions("test-tool", path.join(multi_home, ".local", "share")) as {VersionInfo}
assert(#all_versions == 2, "should find two versions, got " .. #all_versions)

-- Should be sorted with latest first
assert(all_versions[1].version == "2.0.0", "latest version first: " .. all_versions[1].version)
assert(all_versions[2].version == "1.0.0", "older version second: " .. all_versions[2].version)

-- find_latest should return newest
local latest = version.find_latest("test-tool", path.join(multi_home, ".local", "share")) as VersionInfo
assert(latest.version == "2.0.0", "find_latest returns newest: " .. latest.version)
assert(latest.path == v2_bin, "latest path correct")
