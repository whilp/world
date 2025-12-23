package.path = "/workspaces/dotfiles/.config/setup/?.lua;" ..
	"/home/codespace/.local/bootstrap/lib/lua/?.lua;" ..
	"/home/codespace/.local/bootstrap/lib/lua/?/init.lua;;"

local cosmo = require("cosmo") -- luacheck: ignore

print("validating setup scripts...")
print()

local env_module = require("env")
local env = env_module.get()

print("environment:")
print(string.format("  SRC: %s", env.SRC))
print(string.format("  DST: %s", env.DST))
print(string.format("  SHELLINIT: %s", env.SHELLINIT))
print(string.format("  REMOTE: %s", env.REMOTE))
print()

print("checking modules can load:")
local modules = {
	"ai",
	"backup",
	"claude",
	"codespace",
	"extras",
	"git",
	"nvim",
	"shell",
	"setup",
}

local all_ok = true
for _, name in ipairs(modules) do
	local ok, mod = pcall(require, name)
	if ok then
		print(string.format("  ✓ %s", name))
	else
		print(string.format("  ✗ %s: %s", name, tostring(mod)))
		all_ok = false
	end
end

print()
if all_ok then
	print("all modules validated successfully! ✓")
	os.exit(0)
else
	print("validation failed ✗")
	os.exit(1)
end
