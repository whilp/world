local Fuzzy = {}

function Fuzzy.match(str, pattern)
  local strLower = str:lower()
  local patternLower = pattern:lower()
  local patternIdx = 1
  local lastMatchIdx = 0
  local score = 0
  local allConsecutive = true
  local matchStartIdx = nil

  for i = 1, #strLower do
    if patternIdx <= #patternLower and strLower:sub(i, i) == patternLower:sub(patternIdx, patternIdx) then
      local gap = i - lastMatchIdx - 1
      score = score + gap

      if patternIdx == 1 then
        matchStartIdx = i
      end

      if gap == 0 then
        score = score - (10 + patternIdx * 5)
      else
        if patternIdx > 1 then
          allConsecutive = false
        end
      end

      lastMatchIdx = i
      patternIdx = patternIdx + 1
    end
  end

  if patternIdx > #patternLower then
    if allConsecutive and matchStartIdx and
       (matchStartIdx == 1 or strLower:sub(matchStartIdx-1, matchStartIdx-1):match("[%s%-_/]")) then
      score = score - 300
    end

    score = score + math.floor(#strLower / 2)
    return true, score
  end
  return false, math.huge
end

return Fuzzy
