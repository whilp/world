#!/usr/bin/env lua
local cosmo = require("cosmo")

local function fetch_json(url)
  local status, headers, body = cosmo.Fetch(url, {
    headers = {
      ["User-Agent"] = "curl/8.0",
      ["Accept"] = "application/vnd.github+json",
    },
  })

  if status ~= 200 then
    return nil, string.format("HTTP %d: %s", status, body)
  end

  local ok, data = pcall(cosmo.DecodeJson, body)
  if not ok then
    return nil, "failed to parse JSON: " .. tostring(data)
  end

  return data
end

local function extract_pr_info(url_or_num, repo)
  local owner, repo_name, pr_num, review_id

  if url_or_num:match("^https://github%.com/") then
    owner, repo_name, pr_num = url_or_num:match("github%.com/([^/]+)/([^/]+)/pull/(%d+)")
    review_id = url_or_num:match("#pullrequestreview%-(%d+)$")
  elseif url_or_num:match("^%d+$") then
    if not repo then
      return nil, "repository required when using PR number (format: owner/repo)"
    end
    owner, repo_name = repo:match("^([^/]+)/([^/]+)$")
    if not owner or not repo_name then
      return nil, "invalid repository format (expected: owner/repo)"
    end
    pr_num = url_or_num
  else
    return nil, "invalid PR URL or number"
  end

  if not owner or not repo_name or not pr_num then
    return nil, "could not parse PR information from: " .. url_or_num
  end

  return {
    owner = owner,
    repo = repo_name,
    pr = pr_num,
    review = review_id,
  }
end

local function fetch_reviews(info)
  local url = string.format("https://api.github.com/repos/%s/%s/pulls/%s/reviews", info.owner, info.repo, info.pr)

  return fetch_json(url)
end

local function fetch_review_comments(info, review_id)
  local url = string.format(
    "https://api.github.com/repos/%s/%s/pulls/%s/reviews/%s/comments",
    info.owner,
    info.repo,
    info.pr,
    review_id
  )

  return fetch_json(url)
end

local function print_review_comments(info, opts)
  opts = opts or {}
  local writer = opts.writer or print
  local get_reviews = opts.fetch_reviews or fetch_reviews
  local get_comments = opts.fetch_review_comments or fetch_review_comments

  local reviews, err = get_reviews(info)
  if not reviews then
    return nil, err
  end

  if info.review then
    local comments, err = get_comments(info, info.review)
    if not comments then
      return nil, err
    end

    writer(string.format("Review #%s comments:", info.review))
    writer("")

    for i, comment in ipairs(comments) do
      writer(string.format("Comment %d:", i))
      writer(string.format("  File: %s", comment.path or "N/A"))
      if comment.line or comment.original_line then
        writer(string.format("  Line: %s", comment.line or comment.original_line))
      end
      writer(string.format("  %s", comment.body))
      writer("")
    end

    return true
  end

  writer(string.format("Reviews for PR #%s:", info.pr))
  writer("")

  for _, review in ipairs(reviews) do
    writer(string.format("Review ID: %s", review.id))
    writer(string.format("  User: %s", review.user.login))
    writer(string.format("  State: %s", review.state))
    writer(string.format("  Submitted: %s", review.submitted_at))
    if review.body and #review.body > 0 then
      writer(string.format("  Body: %s", review.body))
    end
    writer(string.format("  URL: %s", review.html_url))
    writer("")
  end

  return true
end

local function get_review_data(url_or_num, repo, opts)
  opts = opts or {}
  local get_reviews = opts.fetch_reviews or fetch_reviews
  local get_comments = opts.fetch_review_comments or fetch_review_comments

  local info, err = extract_pr_info(url_or_num, repo)
  if not info then
    return nil, err
  end

  if info.review then
    local comments, err = get_comments(info, info.review)
    if not comments then
      return nil, err
    end
    return {
      type = "review_comments",
      pr = info.pr,
      owner = info.owner,
      repo = info.repo,
      review_id = info.review,
      comments = comments,
    }
  end

  local reviews, err = get_reviews(info)
  if not reviews then
    return nil, err
  end

  return {
    type = "reviews",
    pr = info.pr,
    owner = info.owner,
    repo = info.repo,
    reviews = reviews,
  }
end

local function main(args)
  if #args == 0 or args[1] == "--help" or args[1] == "-h" then
    io.stderr:write([[
usage: gh-review.lua <pr-url-or-number> [repo]

Fetch GitHub PR review comments.

arguments:
  pr-url-or-number    Full PR URL or just the number
  repo               Repository in owner/repo format (required when using PR number)

examples:
  gh-review.lua https://github.com/owner/repo/pull/123
  gh-review.lua https://github.com/owner/repo/pull/123#pullrequestreview-456
  gh-review.lua 123 owner/repo

]])
    return 1
  end

  local pr_input = args[1]
  local repo = args[2]

  local info, err = extract_pr_info(pr_input, repo)
  if not info then
    io.stderr:write("error: " .. err .. "\n")
    return 1
  end

  local ok, err = print_review_comments(info)
  if not ok then
    io.stderr:write("error: " .. err .. "\n")
    return 1
  end

  return 0
end

if ... then
  os.exit(main({ ... }))
else
  return {
    extract_pr_info = extract_pr_info,
    fetch_reviews = fetch_reviews,
    fetch_review_comments = fetch_review_comments,
    print_review_comments = print_review_comments,
    get_review_data = get_review_data,
  }
end
