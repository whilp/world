local lu = require("luaunit")
local review = require("build.review")

TestExtractPrInfoFromUrl = {}

function TestExtractPrInfoFromUrl:test_parses_simple_pr_url()
  local info, err = review.extract_pr_info("https://github.com/owner/repo/pull/123")

  lu.assertNil(err)
  lu.assertEquals(info.owner, "owner")
  lu.assertEquals(info.repo, "repo")
  lu.assertEquals(info.pr, "123")
  lu.assertNil(info.review)
end

function TestExtractPrInfoFromUrl:test_parses_pr_url_with_review_anchor()
  local info, err = review.extract_pr_info(
    "https://github.com/anthropics/claude-code/pull/456#pullrequestreview-789"
  )

  lu.assertNil(err)
  lu.assertEquals(info.owner, "anthropics")
  lu.assertEquals(info.repo, "claude-code")
  lu.assertEquals(info.pr, "456")
  lu.assertEquals(info.review, "789")
end

function TestExtractPrInfoFromUrl:test_handles_numeric_owner_and_repo()
  local info, err = review.extract_pr_info("https://github.com/user123/repo456/pull/789")

  lu.assertNil(err)
  lu.assertEquals(info.owner, "user123")
  lu.assertEquals(info.repo, "repo456")
  lu.assertEquals(info.pr, "789")
end

function TestExtractPrInfoFromUrl:test_handles_hyphenated_names()
  local info, err = review.extract_pr_info("https://github.com/my-org/my-awesome-repo/pull/42")

  lu.assertNil(err)
  lu.assertEquals(info.owner, "my-org")
  lu.assertEquals(info.repo, "my-awesome-repo")
  lu.assertEquals(info.pr, "42")
end

TestExtractPrInfoFromNumber = {}

function TestExtractPrInfoFromNumber:test_parses_pr_number_with_repo()
  local info, err = review.extract_pr_info("123", "owner/repo")

  lu.assertNil(err)
  lu.assertEquals(info.owner, "owner")
  lu.assertEquals(info.repo, "repo")
  lu.assertEquals(info.pr, "123")
  lu.assertNil(info.review)
end

function TestExtractPrInfoFromNumber:test_fails_without_repo()
  local info, err = review.extract_pr_info("123")

  lu.assertNil(info)
  lu.assertStrContains(err, "repository required")
end

function TestExtractPrInfoFromNumber:test_fails_with_invalid_repo_format()
  local info, err = review.extract_pr_info("123", "invalid-repo")

  lu.assertNil(info)
  lu.assertStrContains(err, "invalid repository format")
end

function TestExtractPrInfoFromNumber:test_fails_with_empty_repo()
  local info, err = review.extract_pr_info("123", "")

  lu.assertNil(info)
  lu.assertStrContains(err, "invalid repository format")
end

TestExtractPrInfoErrors = {}

function TestExtractPrInfoErrors:test_rejects_invalid_url()
  local info, err = review.extract_pr_info("not-a-url")

  lu.assertNil(info)
  lu.assertStrContains(err, "invalid PR URL or number")
end

function TestExtractPrInfoErrors:test_rejects_non_github_url()
  local info, err = review.extract_pr_info("https://gitlab.com/owner/repo/merge_requests/123")

  lu.assertNil(info)
  lu.assertStrContains(err, "invalid PR URL or number")
end

function TestExtractPrInfoErrors:test_rejects_github_non_pr_url()
  local info, err = review.extract_pr_info("https://github.com/owner/repo/issues/123")

  lu.assertNil(info)
  lu.assertStrContains(err, "could not parse PR information")
end

TestPrintReviewComments = {}

function TestPrintReviewComments:test_formats_review_list()
  local output = {}
  local function capture(line)
    table.insert(output, line)
  end

  local mock_reviews = {
    {
      id = 12345,
      user = { login = "reviewer1" },
      state = "APPROVED",
      submitted_at = "2024-01-15T10:30:00Z",
      body = "Looks good!",
      html_url = "https://github.com/owner/repo/pull/1#pullrequestreview-12345",
    },
    {
      id = 67890,
      user = { login = "reviewer2" },
      state = "CHANGES_REQUESTED",
      submitted_at = "2024-01-16T14:00:00Z",
      body = "",
      html_url = "https://github.com/owner/repo/pull/1#pullrequestreview-67890",
    },
  }

  local info = { owner = "owner", repo = "repo", pr = "1" }

  local ok, err = review.print_review_comments(info, {
    writer = capture,
    fetch_reviews = function()
      return mock_reviews
    end,
  })

  lu.assertTrue(ok, "should succeed: " .. tostring(err))
  lu.assertTrue(#output > 0, "should produce output")

  local full_output = table.concat(output, "\n")
  lu.assertStrContains(full_output, "Reviews for PR #1:")
  lu.assertStrContains(full_output, "Review ID: 12345")
  lu.assertStrContains(full_output, "User: reviewer1")
  lu.assertStrContains(full_output, "State: APPROVED")
  lu.assertStrContains(full_output, "Body: Looks good!")
  lu.assertStrContains(full_output, "Review ID: 67890")
  lu.assertStrContains(full_output, "State: CHANGES_REQUESTED")
end

function TestPrintReviewComments:test_formats_specific_review_comments()
  local output = {}
  local function capture(line)
    table.insert(output, line)
  end

  local mock_reviews = {
    { id = 12345, body = "Please fix these issues", user = { login = "reviewer" }, state = "CHANGES_REQUESTED" },
  }
  local mock_comments = {
    {
      path = "src/main.lua",
      line = 42,
      body = "Consider using a constant here",
    },
    {
      path = "src/utils.lua",
      original_line = 100,
      body = "This could be simplified",
    },
    {
      path = "README.md",
      body = "Typo in documentation",
    },
  }

  local info = { owner = "owner", repo = "repo", pr = "1", review = "12345" }

  local ok, err = review.print_review_comments(info, {
    writer = capture,
    fetch_reviews = function()
      return mock_reviews
    end,
    fetch_review_comments = function()
      return mock_comments
    end,
  })

  lu.assertTrue(ok, "should succeed: " .. tostring(err))

  local full_output = table.concat(output, "\n")
  lu.assertStrContains(full_output, "Review #12345:")
  lu.assertStrContains(full_output, "Summary:")
  lu.assertStrContains(full_output, "Please fix these issues")
  lu.assertStrContains(full_output, "Inline comments:")
  lu.assertStrContains(full_output, "File: src/main.lua")
  lu.assertStrContains(full_output, "Line: 42")
  lu.assertStrContains(full_output, "Consider using a constant here")
  lu.assertStrContains(full_output, "File: src/utils.lua")
  lu.assertStrContains(full_output, "Line: 100")
  lu.assertStrContains(full_output, "File: README.md")
  lu.assertStrContains(full_output, "Typo in documentation")
end

function TestPrintReviewComments:test_handles_fetch_error()
  local output = {}
  local function capture(line)
    table.insert(output, line)
  end

  local info = { owner = "owner", repo = "repo", pr = "1" }

  local ok, err = review.print_review_comments(info, {
    writer = capture,
    fetch_reviews = function()
      return nil, "HTTP 404: Not Found"
    end,
  })

  lu.assertNil(ok)
  lu.assertStrContains(err, "HTTP 404")
end

TestParseLinkHeader = {}

function TestParseLinkHeader:test_extracts_next_url()
  local review = require("build.review")
  local link = '<https://api.github.com/repos/o/r/pulls/1/reviews?page=2>; rel="next", <https://api.github.com/repos/o/r/pulls/1/reviews?page=5>; rel="last"'
  local next_url = review.parse_link_header(link)
  lu.assertEquals(next_url, "https://api.github.com/repos/o/r/pulls/1/reviews?page=2")
end

function TestParseLinkHeader:test_returns_nil_when_no_next()
  local review = require("build.review")
  local link = '<https://api.github.com/repos/o/r/pulls/1/reviews?page=1>; rel="first"'
  local next_url = review.parse_link_header(link)
  lu.assertNil(next_url)
end

function TestParseLinkHeader:test_handles_nil_header()
  local review = require("build.review")
  local next_url = review.parse_link_header(nil)
  lu.assertNil(next_url)
end

TestGetReviewData = {}

function TestGetReviewData:test_returns_reviews_for_pr_url()
  local mock_reviews = {
    {
      id = 111,
      user = { login = "alice" },
      state = "APPROVED",
      body = "LGTM",
    },
  }

  local data, err = review.get_review_data(
    "https://github.com/myorg/myrepo/pull/42",
    nil,
    {
      fetch_reviews = function()
        return mock_reviews
      end,
    }
  )

  lu.assertNil(err)
  lu.assertEquals(data.type, "reviews")
  lu.assertEquals(data.owner, "myorg")
  lu.assertEquals(data.repo, "myrepo")
  lu.assertEquals(data.pr, "42")
  lu.assertEquals(#data.reviews, 1)
  lu.assertEquals(data.reviews[1].user.login, "alice")
end

function TestGetReviewData:test_returns_comments_for_review_url()
  local mock_reviews = {
    { id = 555, body = "Please address these issues", user = { login = "reviewer" }, state = "CHANGES_REQUESTED" },
  }
  local mock_comments = {
    { path = "main.lua", line = 10, body = "Fix this" },
    { path = "util.lua", line = 20, body = "Add test" },
  }

  local data, err = review.get_review_data(
    "https://github.com/owner/repo/pull/99#pullrequestreview-555",
    nil,
    {
      fetch_reviews = function()
        return mock_reviews
      end,
      fetch_review_comments = function()
        return mock_comments
      end,
    }
  )

  lu.assertNil(err)
  lu.assertEquals(data.type, "review_comments")
  lu.assertEquals(data.owner, "owner")
  lu.assertEquals(data.repo, "repo")
  lu.assertEquals(data.pr, "99")
  lu.assertEquals(data.review_id, "555")
  lu.assertEquals(data.body, "Please address these issues")
  lu.assertEquals(#data.comments, 2)
  lu.assertEquals(data.comments[1].path, "main.lua")
  lu.assertEquals(data.comments[2].body, "Add test")
end

function TestGetReviewData:test_accepts_pr_number_with_repo()
  local mock_reviews = {{ id = 1, user = { login = "bob" }, state = "PENDING" }}

  local data, err = review.get_review_data("77", "acme/widgets", {
    fetch_reviews = function()
      return mock_reviews
    end,
  })

  lu.assertNil(err)
  lu.assertEquals(data.type, "reviews")
  lu.assertEquals(data.owner, "acme")
  lu.assertEquals(data.repo, "widgets")
  lu.assertEquals(data.pr, "77")
end

function TestGetReviewData:test_returns_error_for_invalid_url()
  local data, err = review.get_review_data("not-a-valid-url")

  lu.assertNil(data)
  lu.assertStrContains(err, "invalid")
end

function TestGetReviewData:test_returns_error_on_fetch_failure()
  local data, err = review.get_review_data(
    "https://github.com/owner/repo/pull/1",
    nil,
    {
      fetch_reviews = function()
        return nil, "rate limited"
      end,
    }
  )

  lu.assertNil(data)
  lu.assertStrContains(err, "rate limited")
end
