local Fuzzy = {}

local TYPE_PRIORITY = {
	running_app = 10,
	window = 8,
	installed_app = 2,
	command = 1,
}

local function normalize(s)
	if not s then
		return ""
	end
	return s:lower()
end

local function fuzzy_score_dp(haystack, needle)
	haystack = normalize(haystack)
	needle = normalize(needle)

	local n = #haystack
	local m = #needle

	if m == 0 or m > n then
		return nil
	end

	-- quick subsequence check to early-out
	do
		local i = 1
		for j = 1, n do
			if haystack:sub(j, j) == needle:sub(i, i) then
				i = i + 1
				if i > m then
					break
				end
			end
		end
		if i <= m then
			return nil
		end
	end

	local NEG_INF = -math.huge

	local function is_word_start_idx(j)
		if j == 1 then
			return true
		end
		local prev = haystack:sub(j - 1, j - 1)
		return prev:match("[^%w]") ~= nil
	end

	-- Per-character score.
	-- Consecutive matches get an extra bonus.
	local function char_score(j, consecutive)
		local s = 10

		if is_word_start_idx(j) then
			s = s + 40 -- word start bonus (tuned from 15)
		end
		if j == 1 then
			s = s + 10 -- start-of-string bonus (tuned from 5)
		end

		-- earlier positions are better, but with reduced bias
		s = s + math.max(0, 15 - j / 2)

		if consecutive then
			s = s + 20 -- adjacency bonus (tuned from 15)
		end

		return s
	end

	-- DP arrays for previous and current pattern row
	local D_prev, M_prev = {}, {}
	local D_curr, M_curr = {}, {}

	-- i = 1 (first pattern char)
	local p1 = needle:sub(1, 1)
	for j = 1, n do
		if haystack:sub(j, j) == p1 then
			D_prev[j] = char_score(j, false)
		else
			D_prev[j] = NEG_INF
		end

		if j == 1 then
			M_prev[j] = D_prev[j]
		else
			M_prev[j] = math.max(M_prev[j - 1], D_prev[j])
		end
	end

	-- i >= 2
	for i_idx = 2, m do
		local c = needle:sub(i_idx, i_idx)

		-- reset current row
		for j = 1, n do
			D_curr[j] = NEG_INF
			M_curr[j] = NEG_INF
		end

		for j = 1, n do
			if haystack:sub(j, j) == c then
				local best_non_consecutive = NEG_INF
				local best_consecutive = NEG_INF

				if j > 1 and M_prev[j - 1] > NEG_INF then
					best_non_consecutive = M_prev[j - 1] + char_score(j, false)
				end

				if j > 1 and D_prev[j - 1] > NEG_INF then
					best_consecutive = D_prev[j - 1] + char_score(j, true)
				end

				local best = best_non_consecutive
				if best_consecutive > best then
					best = best_consecutive
				end

				D_curr[j] = best
			else
				D_curr[j] = NEG_INF
			end

			if j == 1 then
				M_curr[j] = D_curr[j]
			else
				M_curr[j] = math.max(M_curr[j - 1], D_curr[j])
			end
		end

		-- roll rows
		D_prev, D_curr = D_curr, D_prev
		M_prev, M_curr = M_curr, M_prev
	end

	local best = M_prev[n]
	if best == NEG_INF then
		return nil
	end

	-- Extra bonuses for full substring / prefix matches,
	-- on top of the DP subsequence score.
	if haystack:sub(1, m) == needle then
		best = best + 50 -- strong prefix bonus (tuned from 30)
	end
	if haystack:find(needle, 1, true) then
		best = best + 20 -- substring bonus (tuned from 15)
	end

	-- Word-level matching bonuses
	local normalized_haystack = normalize(haystack)
	local normalized_needle = normalize(needle)
	for word in normalized_haystack:gmatch("%w+") do
		if word == normalized_needle then
			best = best + 150 -- exact word match
			break
		end
		if #word >= #normalized_needle and word:sub(1, #normalized_needle) == normalized_needle then
			best = best + 100 -- prefix of a word
			break
		end
	end

	return best
end

local function split_query(query)
	local tokens = {}
	for token in query:gmatch("%S+") do
		table.insert(tokens, token)
	end
	return tokens
end

local function match_multi_token(item, tokens)
	local text = normalize(item.text or "")
	local subtext = normalize(item.subText or "")

	local unmatched_tokens = {}
	for _, token in ipairs(tokens) do
		table.insert(unmatched_tokens, normalize(token))
	end

	-- Try to match subtext tokens first
	local matched_in_subtext = {}
	for i = #unmatched_tokens, 1, -1 do
		local token = unmatched_tokens[i]
		if subtext:find(token, 1, true) then
			table.insert(matched_in_subtext, token)
			table.remove(unmatched_tokens, i)
		end
	end

	-- Remaining tokens must match in text
	if #unmatched_tokens > 0 then
		local remaining_query = table.concat(unmatched_tokens, " ")
		local text_score = fuzzy_score_dp(item.text or "", remaining_query)
		if not text_score then
			return nil
		end

		-- Bonus for matching subtext tokens exactly
		local subtext_bonus = #matched_in_subtext * 30
		return text_score + subtext_bonus
	end

	return nil
end

function Fuzzy.match_item(item, query, subtext_penalty)
	subtext_penalty = subtext_penalty or 50
	local subtext_only_penalty = 25

	local text_score = fuzzy_score_dp(item.text or "", query)
	local subtext_score = fuzzy_score_dp(item.subText or "", query)
	local combined_score = fuzzy_score_dp((item.text or "") .. " " .. (item.subText or ""), query)

	local best_score = nil

	if text_score then
		best_score = text_score
	end

	if subtext_score then
		local adjusted = subtext_score - subtext_only_penalty
		if not best_score or adjusted > best_score then
			best_score = adjusted
		end
	end

	if combined_score then
		local adjusted = combined_score - subtext_penalty
		if not best_score or adjusted > best_score then
			best_score = adjusted
		end
	end

	-- Try multi-token matching (e.g., "slack unread")
	local tokens = split_query(query)
	if #tokens > 1 then
		local multi_token_score = match_multi_token(item, tokens)
		if multi_token_score then
			if not best_score or multi_token_score > best_score then
				best_score = multi_token_score
			end
		end
	end

	return best_score
end

local function get_app_adjustment(item, app_adjustments)
	if not app_adjustments then
		return 0
	end

	local app_name = nil
	if item.original then
		-- For commands, check text field first (before subText check)
		if item.original.commandId and item.original.text then
			app_name = item.original.text
		-- For apps (running or installed), check appName
		elseif item.original.appName then
			app_name = item.original.appName
		-- For windows, extract app name from subText
		elseif item.original.subText then
			app_name = item.original.subText:match("^([^%-]+)") -- Extract app name before " - "
			if app_name then
				app_name = app_name:gsub("^%s*(.-)%s*$", "%1") -- Trim whitespace
			end
		end
	end

	if app_name then
		return app_adjustments[app_name] or 0
	end

	return 0
end

function Fuzzy.fuzzy_find(items, query, max_results, subtext_penalty, app_adjustments)
	max_results = max_results or 10
	subtext_penalty = subtext_penalty or 50
	app_adjustments = app_adjustments or {}

	local scored = {}
	for _, item in ipairs(items) do
		local score = Fuzzy.match_item(item, query, subtext_penalty)
		if score then
			local type_priority = TYPE_PRIORITY[item.type] or 0
			local final_score = score + (type_priority * 15)

			-- Add MRU (most recently used) bonus for windows
			-- First window gets +100, decaying by 2 points per position
			-- This makes recent windows rank higher without overwhelming fuzzy match scores
			if item.original and item.original.mruIndex then
				local mru_bonus = math.max(0, 100 - (item.original.mruIndex * 2))
				final_score = final_score + mru_bonus
			end

			-- Apply app-specific score adjustments
			local adjustment = get_app_adjustment(item, app_adjustments)
			final_score = final_score + adjustment

			table.insert(scored, {
				item = item,
				matchScore = final_score,
			})
		end
	end

	table.sort(scored, function(a, b)
		return a.matchScore > b.matchScore
	end)

	local results = {}
	for i = 1, math.min(max_results, #scored) do
		local entry = scored[i]
		local result = {}
		for k, v in pairs(entry.item) do
			result[k] = v
		end
		result.matchScore = entry.matchScore
		table.insert(results, result)
	end

	return results
end

return Fuzzy
