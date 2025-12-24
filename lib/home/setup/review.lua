package.path = "/workspaces/dotfiles/.config/setup/?.lua;" .. package.path

local checks = {
	{
		name = "env module",
		tests = {
			{desc = "exports get()", check = function()
				local env = require("env")
				return type(env.get) == "function"
			end},
			{desc = "get() returns table with DST, SRC, PATH", check = function()
				local env = require("env")
				local e = env.get()
				return e.DST and e.SRC and e.PATH and e.LUA_PATH and e.SHELLINIT and e.REMOTE ~= nil
			end},
		}
	},
	{
		name = "setup modules",
		tests = {
			{desc = "all modules export run()", check = function()
				local modules = {"ai", "backup", "claude", "codespace", "extras", "git", "nvim", "shell"}
				for _, name in ipairs(modules) do
					local m = require(name)
					if type(m.run) ~= "function" then
						return false, name .. " missing run()"
					end
				end
				return true
			end},
			{desc = "setup exports main()", check = function()
				local setup = require("setup")
				return type(setup.main) == "function"
			end},
		}
	},
}

print("manual review checklist:")
print()

local total = 0
local passed = 0

for _, group in ipairs(checks) do
	print(string.format("• %s", group.name))
	for _, test in ipairs(group.tests) do
		total = total + 1
		local ok, err = test.check()
		if ok then
			print(string.format("  ✓ %s", test.desc))
			passed = passed + 1
		else
			print(string.format("  ✗ %s", test.desc))
			if err then
				print(string.format("    %s", err))
			end
		end
	end
end

print()
print(string.format("passed: %d/%d", passed, total))

if passed == total then
	print()
	print("all checks passed! ✓")
	os.exit(0)
else
	os.exit(1)
end
