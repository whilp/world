-- bootstrap-home - fetch and exec home binary for current platform

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")

local record Shas
	["darwin-arm64"]: string
	["linux-arm64"]: string
	["linux-x86_64"]: string
end

-- Generated at build time - contains SHA256 for each platform
local SHAS: Shas = require("bootstrap-home.shas")
local REPO = "https://github.com/whilp/world"

local function normalize_os(os_name: string): string
	if os_name == "LINUX" then
		return "linux"
	elseif os_name == "DARWIN" or os_name == "XNU" then
		return "darwin"
	end
	return os_name:lower()
end

local function normalize_arch(arch: string): string
	if arch == "X86_64" or arch == "AMD64" then
		return "x86_64"
	elseif arch == "AARCH64" or arch == "ARM64" then
		return "arm64"
	end
	return arch:lower()
end

local function detect_platform(): string, string
	local os_name = cosmo.GetHostOs()
	local arch = cosmo.GetHostIsa()

	if not os_name or not arch then
		return nil, "failed to detect platform"
	end

	local platform = normalize_os(os_name) .. "-" .. normalize_arch(arch)
	return platform
end

local function fetch_latest_release_tag(): string, string
	local api_url = "https://api.github.com/repos/whilp/world/releases/latest"
	local status, _, body = cosmo.Fetch(api_url, {
		headers = {Accept = "application/vnd.github+json"},
	})

	if not status or status ~= 200 then
		return nil, "failed to fetch latest release info: HTTP " .. tostring(status)
	end

	local release = cosmo.DecodeJson(body) as {tag_name: string}
	if not release or not release.tag_name then
		return nil, "failed to parse release info"
	end

	return release.tag_name
end

local function download_home(platform: string, tag: string): string, string
	local sha256 = SHAS[platform]
	if not sha256 then
		return nil, "unsupported platform: " .. platform
	end

	local filename = "home-" .. platform
	local url = REPO .. "/releases/download/" .. tag .. "/" .. filename

	io.stderr:write("Downloading " .. filename .. " from " .. tag .. "...\n")

	local status, _, body = cosmo.Fetch(url, {
		maxresponse = 100 * 1024 * 1024, -- 100MB limit
	})

	if not status or status ~= 200 then
		return nil, "failed to download: HTTP " .. tostring(status)
	end

	-- Verify SHA256
	local actual_sha = cosmo.EncodeHex(cosmo.Sha256(body)):lower()
	if actual_sha ~= sha256 then
		return nil, string.format(
			"SHA256 mismatch: expected %s, got %s",
			sha256,
			actual_sha
		)
	end

	return body
end

local function write_executable(content: string, dest: string): boolean, string
	local fd = unix.open(dest, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("0755", 8))
	if not fd or fd < 0 then
		return nil, "failed to create file: " .. dest
	end

	local written = unix.write(fd, content)
	unix.close(fd)

	if not written or written < 0 then
		return nil, "failed to write file: " .. dest
	end

	return true
end

local function main(args: {string}): number
	-- Detect platform
	local platform, err = detect_platform()
	if not platform then
		io.stderr:write("error: " .. err .. "\n")
		return 1
	end

	-- Get latest release tag
	local tag, tag_err = fetch_latest_release_tag()
	if not tag then
		io.stderr:write("error: " .. tag_err .. "\n")
		return 1
	end

	-- Download home binary
	local content, dl_err = download_home(platform, tag)
	if not content then
		io.stderr:write("error: " .. dl_err .. "\n")
		return 1
	end

	-- Write to temp file
	local temp_dir = os.getenv("TMPDIR") or "/tmp"
	local temp_file = path.join(temp_dir, "home-" .. platform .. "-" .. os.time())

	local ok, write_err = write_executable(content, temp_file)
	if not ok then
		io.stderr:write("error: " .. write_err .. "\n")
		return 1
	end

	-- Exec the binary with -v -f
	local exec_args = {temp_file, "-v", "-f"}
	for _, arg in ipairs(args) do
		table.insert(exec_args, arg)
	end

	unix.execv(temp_file, exec_args)

	-- If we get here, exec failed
	io.stderr:write("error: failed to exec " .. temp_file .. "\n")
	return 1
end

if cosmo.is_main() then
	os.exit(main(arg))
end

return {
	main = main,
	_VERSION = "0.1.0",
	_DESCRIPTION = "Bootstrap home binary for current platform",
}
