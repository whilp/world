local Fuzzy = {}

local TYPE_PRIORITY = {
	window = 3,
	app = 2,
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

	return best_score
end

function Fuzzy.fuzzy_find(items, query, max_results, subtext_penalty)
	max_results = max_results or 10
	subtext_penalty = subtext_penalty or 50

	local scored = {}
	for _, item in ipairs(items) do
		local score = Fuzzy.match_item(item, query, subtext_penalty)
		if score then
			local type_priority = TYPE_PRIORITY[item.type] or 0
			local final_score = score + (type_priority * 10)
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
