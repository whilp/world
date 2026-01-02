local lu = require("luaunit")
local action = require("build.action")

TestActionParse = {}

function TestActionParse:test_parse_with_sha_and_version()
	local result = action.parse("actions/checkout@08eba0b27e820071cde6df949e0beb9ba4906955 # v4.3.0")
	lu.assertNotNil(result)
	lu.assertEquals(result.owner, "actions")
	lu.assertEquals(result.repo, "checkout")
	lu.assertEquals(result.ref, "08eba0b27e820071cde6df949e0beb9ba4906955")
	lu.assertEquals(result.is_sha, true)
	lu.assertEquals(result.version, "v4.3.0")
end

function TestActionParse:test_parse_with_different_action()
	local result = action.parse("actions/upload-artifact@6f51ac03b9356f520e9adb1b1b7802705f340c2b # v4.5.0")
	lu.assertNotNil(result)
	lu.assertEquals(result.owner, "actions")
	lu.assertEquals(result.repo, "upload-artifact")
	lu.assertEquals(result.ref, "6f51ac03b9356f520e9adb1b1b7802705f340c2b")
	lu.assertEquals(result.is_sha, true)
	lu.assertEquals(result.version, "v4.5.0")
end

function TestActionParse:test_parse_with_version_tag()
	local result = action.parse("actions/checkout@v4")
	lu.assertNotNil(result)
	lu.assertEquals(result.owner, "actions")
	lu.assertEquals(result.repo, "checkout")
	lu.assertEquals(result.ref, "v4")
	lu.assertEquals(result.is_sha, false)
	lu.assertNil(result.version)
end

function TestActionParse:test_parse_with_short_sha()
	local result = action.parse("actions/checkout@abc123")
	lu.assertNotNil(result)
	lu.assertEquals(result.owner, "actions")
	lu.assertEquals(result.repo, "checkout")
	lu.assertEquals(result.ref, "abc123")
	lu.assertEquals(result.is_sha, false)
end

function TestActionParse:test_parse_third_party_action()
	local result = action.parse("docker/build-push-action@v5")
	lu.assertNotNil(result)
	lu.assertEquals(result.owner, "docker")
	lu.assertEquals(result.repo, "build-push-action")
	lu.assertEquals(result.ref, "v5")
	lu.assertEquals(result.is_sha, false)
end

function TestActionParse:test_parse_with_whitespace()
	local result = action.parse("  actions/checkout@v4  #  v4.3.0  ")
	lu.assertNotNil(result)
	lu.assertEquals(result.owner, "actions")
	lu.assertEquals(result.repo, "checkout")
	lu.assertEquals(result.ref, "v4")
	lu.assertEquals(result.version, "v4.3.0")
end

function TestActionParse:test_parse_empty_string()
	local result, err = action.parse("")
	lu.assertNil(result)
	lu.assertEquals(err, "empty action string")
end

function TestActionParse:test_parse_nil()
	local result, err = action.parse(nil)
	lu.assertNil(result)
	lu.assertEquals(err, "empty action string")
end

function TestActionParse:test_parse_missing_at()
	local result, err = action.parse("actions/checkout")
	lu.assertNil(result)
	lu.assertEquals(err, "invalid action format")
end

function TestActionParse:test_parse_missing_slash()
	local result, err = action.parse("checkout@v4")
	lu.assertNil(result)
	lu.assertEquals(err, "invalid owner/repo format")
end

TestWorkflowFile = {}

function TestWorkflowFile:test_parse_workflow_file()
	local actions = action.parse_workflow_file(".github/workflows/home.yml")
	lu.assertNotNil(actions)
	lu.assertTrue(#actions > 0)

	lu.assertEquals(actions[1].owner, "actions")
	lu.assertEquals(actions[1].repo, "checkout")
	lu.assertEquals(actions[1].ref, "08eba0b27e820071cde6df949e0beb9ba4906955")
	lu.assertEquals(actions[1].is_sha, true)
	lu.assertEquals(actions[1].version, "v4.3.0")
end

function TestWorkflowFile:test_parse_workflow_finds_all_actions()
	local actions = action.parse_workflow_file(".github/workflows/home.yml")
	lu.assertNotNil(actions)

	local action_names = {}
	for _, act in ipairs(actions) do
		local full_name = act.owner .. "/" .. act.repo
		action_names[full_name] = (action_names[full_name] or 0) + 1
	end

	lu.assertTrue(action_names["actions/checkout"] > 0)
	lu.assertTrue(action_names["actions/upload-artifact"] > 0)
	lu.assertTrue(action_names["actions/download-artifact"] > 0)
end

function TestWorkflowFile:test_parse_nonexistent_file()
	local actions, err = action.parse_workflow_file("nonexistent.yml")
	lu.assertNil(actions)
	lu.assertNotNil(err)
end

os.exit(lu.LuaUnit.run())
