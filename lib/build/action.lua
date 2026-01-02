local M = {}

function M.parse(action_string)
	if not action_string or action_string == "" then
		return nil, "empty action string"
	end

	local trimmed = action_string:match("^%s*(.-)%s*$")
	local action_part, comment_part = trimmed:match("^(.-)%s*#%s*(.+)$")
	if not action_part then
		action_part = trimmed
	end

	local owner_repo, ref = action_part:match("^([^@]+)@(.+)$")
	if not owner_repo or not ref then
		return nil, "invalid action format"
	end

	local owner, repo = owner_repo:match("^([^/]+)/(.+)$")
	if not owner or not repo then
		return nil, "invalid owner/repo format"
	end

	local is_sha = ref:match("^%x+$") and #ref == 40

	local result = {
		owner = owner,
		repo = repo,
		ref = ref,
		is_sha = is_sha or false,
	}

	if comment_part then
		result.version = comment_part:match("^%s*(.-)%s*$")
	end

	return result
end

function M.parse_workflow_file(file_path)
	local file, err = io.open(file_path, "r")
	if not file then
		return nil, err
	end

	local actions = {}
	for line in file:lines() do
		local uses = line:match("uses:%s*(.+)")
		if uses then
			local action, parse_err = M.parse(uses)
			if action then
				table.insert(actions, action)
			end
		end
	end

	file:close()
	return actions
end

return M
