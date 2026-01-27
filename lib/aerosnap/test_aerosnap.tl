#!/usr/bin/env run-test.lua
--check:false - test uses local type definitions that conflict with source module types
local sqlite3 = require("cosmo.lsqlite3")

local aerosnap = require("aerosnap")

local function test_finds_exact_match()
  local db = sqlite3.open(":memory:")
  db:exec([[
    create table window_mappings (
      id integer primary key autoincrement,
      app_bundle_id text not null,
      app_name text,
      window_title text not null,
      workspace text not null,
      updated_at text default current_timestamp
    );
    create unique index idx_bundle_title on window_mappings(app_bundle_id, window_title);
  ]])

  db:exec([[
    insert into window_mappings (app_bundle_id, app_name, window_title, workspace)
    values ('com.test.app', 'Test App', 'My Window', '3')
  ]])

  local workspace, match_type = aerosnap.lookup_workspace(db, "com.test.app", "My Window")

  assert(workspace == "3", "should find workspace 3")
  assert(match_type == "exact", "should be exact match")

  db:close()
end
test_finds_exact_match()

local function test_finds_bundle_fallback()
  local db = sqlite3.open(":memory:")
  db:exec([[
    create table window_mappings (
      id integer primary key autoincrement,
      app_bundle_id text not null,
      app_name text,
      window_title text not null,
      workspace text not null,
      updated_at text default current_timestamp
    );
    create unique index idx_bundle_title on window_mappings(app_bundle_id, window_title);
  ]])

  db:exec([[
    insert into window_mappings (app_bundle_id, app_name, window_title, workspace)
    values ('com.test.app', 'Test App', 'Different Window', '2')
  ]])

  local workspace, match_type = aerosnap.lookup_workspace(db, "com.test.app", "New Window Title")

  assert(workspace == "2", "should find workspace via bundle fallback")
  assert(match_type == "bundle", "should be bundle match")

  db:close()
end
test_finds_bundle_fallback()

local function test_prefers_exact_over_bundle()
  local db = sqlite3.open(":memory:")
  db:exec([[
    create table window_mappings (
      id integer primary key autoincrement,
      app_bundle_id text not null,
      app_name text,
      window_title text not null,
      workspace text not null,
      updated_at text default current_timestamp
    );
    create unique index idx_bundle_title on window_mappings(app_bundle_id, window_title);
  ]])

  db:exec([[
    insert into window_mappings (app_bundle_id, app_name, window_title, workspace)
    values ('com.test.app', 'Test App', 'Other Window', '1')
  ]])
  db:exec([[
    insert into window_mappings (app_bundle_id, app_name, window_title, workspace)
    values ('com.test.app', 'Test App', 'Target Window', '5')
  ]])

  local workspace, match_type = aerosnap.lookup_workspace(db, "com.test.app", "Target Window")

  assert(workspace == "5", "should prefer exact title match")
  assert(match_type == "exact", "should be exact match")

  db:close()
end
test_prefers_exact_over_bundle()

local function test_returns_nil_for_unknown_bundle()
  local db = sqlite3.open(":memory:")
  db:exec([[
    create table window_mappings (
      id integer primary key autoincrement,
      app_bundle_id text not null,
      app_name text,
      window_title text not null,
      workspace text not null,
      updated_at text default current_timestamp
    );
    create unique index idx_bundle_title on window_mappings(app_bundle_id, window_title);
  ]])

  local workspace, match_type = aerosnap.lookup_workspace(db, "com.unknown.app", "Any Window")

  assert(workspace == nil, "should return nil for unknown bundle")
  assert(match_type == nil, "match_type should be nil")

  db:close()
end
test_returns_nil_for_unknown_bundle()
