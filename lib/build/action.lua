local function parse(action_string)
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

local function parse_workflow(workflow_content)
	local actions = {}
	for line in workflow_content:gmatch("[^\r\n]+") do
		local uses = line:match("uses:%s*(.+)")
		if uses then
			local action = parse(uses)
			if action then
				table.insert(actions, action)
			end
		end
	end
	return actions
end

local function parse_workflow_file(file_path)
	local file, err = io.open(file_path, "r")
	if not file then
		return nil, err
	end

	local content = file:read("*a")
	file:close()

	return parse_workflow(content)
end

return {
	parse = parse,
	parse_workflow = parse_workflow,
	parse_workflow_file = parse_workflow_file,
}
