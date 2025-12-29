local lu = require("luaunit")
local sqlite3 = require("cosmo.lsqlite3")

local aerosnap = require("aerosnap")

TestLookupWorkspace = {}

function TestLookupWorkspace:setUp()
  self.db = sqlite3.open(":memory:")
  self.db:exec([[
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
end

function TestLookupWorkspace:tearDown()
  self.db:close()
end

function TestLookupWorkspace:test_finds_exact_match()
  self.db:exec([[
    insert into window_mappings (app_bundle_id, app_name, window_title, workspace)
    values ('com.test.app', 'Test App', 'My Window', '3')
  ]])

  local workspace, match_type = aerosnap.lookup_workspace(self.db, "com.test.app", "My Window")

  lu.assertEquals(workspace, "3", "should find workspace 3")
  lu.assertEquals(match_type, "exact", "should be exact match")
end

function TestLookupWorkspace:test_finds_bundle_fallback()
  self.db:exec([[
    insert into window_mappings (app_bundle_id, app_name, window_title, workspace)
    values ('com.test.app', 'Test App', 'Different Window', '2')
  ]])

  local workspace, match_type = aerosnap.lookup_workspace(self.db, "com.test.app", "New Window Title")

  lu.assertEquals(workspace, "2", "should find workspace via bundle fallback")
  lu.assertEquals(match_type, "bundle", "should be bundle match")
end

function TestLookupWorkspace:test_prefers_exact_over_bundle()
  self.db:exec([[
    insert into window_mappings (app_bundle_id, app_name, window_title, workspace)
    values ('com.test.app', 'Test App', 'Other Window', '1')
  ]])
  self.db:exec([[
    insert into window_mappings (app_bundle_id, app_name, window_title, workspace)
    values ('com.test.app', 'Test App', 'Target Window', '5')
  ]])

  local workspace, match_type = aerosnap.lookup_workspace(self.db, "com.test.app", "Target Window")

  lu.assertEquals(workspace, "5", "should prefer exact title match")
  lu.assertEquals(match_type, "exact", "should be exact match")
end

function TestLookupWorkspace:test_returns_nil_for_unknown_bundle()
  local workspace, match_type = aerosnap.lookup_workspace(self.db, "com.unknown.app", "Any Window")

  lu.assertNil(workspace, "should return nil for unknown bundle")
  lu.assertNil(match_type, "match_type should be nil")
end

os.exit(lu.LuaUnit.run())
