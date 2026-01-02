# GitHub API access via cosmo.Fetch proxy

Investigation into what GitHub API operations are available through the authenticating proxy using cosmo.Fetch.

## Environment

The environment provides an HTTPS proxy with JWT authentication:

```
HTTPS_PROXY=http://container_...:jwt_eyJ...@21.0.0.211:15004
```

cosmo.Fetch automatically uses this proxy for outbound HTTPS requests. The proxy handles network-level authentication but does not inject GitHub API credentials.

## Summary

| Operation | Status | Notes |
|-----------|--------|-------|
| Public GET endpoints | ✅ works | Full read access |
| Protected GET endpoints | ❌ 401/403 | Logs, secrets, collaborators |
| POST (comments) | ❌ 401 | Requires authentication |
| PATCH (update PR) | ❌ 401 | Requires authentication |

## Public endpoints (read-only)

All standard public GitHub API endpoints work:

```lua
local cosmo = require("cosmo")

local headers = {
  ["Accept"] = "application/vnd.github+json",
  ["X-GitHub-Api-Version"] = "2022-11-28",
}

local status, _, body = cosmo.Fetch(url, { headers = headers })
```

### Repository info

| Endpoint | Path |
|----------|------|
| repo info | `/repos/{owner}/{repo}` |
| commits | `/repos/{owner}/{repo}/commits` |
| branches | `/repos/{owner}/{repo}/branches` |
| tags | `/repos/{owner}/{repo}/tags` |
| releases | `/repos/{owner}/{repo}/releases` |
| contents | `/repos/{owner}/{repo}/contents` |

### Pull requests and issues

| Endpoint | Path |
|----------|------|
| list PRs | `/repos/{owner}/{repo}/pulls` |
| PR details | `/repos/{owner}/{repo}/pulls/{number}` |
| list issues | `/repos/{owner}/{repo}/issues` |
| issue comments | `/repos/{owner}/{repo}/issues/{number}/comments` |
| review comments | `/repos/{owner}/{repo}/pulls/{number}/comments` |
| reviews | `/repos/{owner}/{repo}/pulls/{number}/reviews` |

### GitHub Actions

| Endpoint | Path |
|----------|------|
| workflows | `/repos/{owner}/{repo}/actions/workflows` |
| workflow runs | `/repos/{owner}/{repo}/actions/runs` |
| job info | `/repos/{owner}/{repo}/actions/jobs/{job_id}` |

Job info includes status, conclusion, timing, and step details:

```json
{
  "id": 59298082655,
  "status": "completed",
  "conclusion": "success",
  "steps": [
    {"name": "Set up job", "status": "completed", "conclusion": "success"},
    {"name": "Build home and lua binaries", "status": "completed", "conclusion": "success"}
  ]
}
```

## Protected endpoints (blocked)

These require authentication and return 401 or 403:

| Endpoint | Status | Error |
|----------|--------|-------|
| job logs | 403 | "Must have admin rights to Repository" |
| secrets | 401 | "Requires authentication" |
| collaborators | 401 | "Requires authentication" |

## Write operations (blocked)

All write operations require a `GITHUB_TOKEN`:

```lua
-- POST comment - returns 401
cosmo.Fetch(url, {
  method = "POST",
  headers = headers,
  body = cosmo.EncodeJson({ body = "comment text" }),
})

-- PATCH PR title - returns 401
cosmo.Fetch(url, {
  method = "PATCH",
  headers = headers,
  body = cosmo.EncodeJson({ title = "new title" }),
})
```

## Example: reading PR review comments

```lua
#!/usr/bin/env lua
local cosmo = require("cosmo")

local owner = "whilp"
local repo = "world"
local pr_number = 183

local headers = {
  ["Accept"] = "application/vnd.github+json",
  ["X-GitHub-Api-Version"] = "2022-11-28",
}

local url = string.format(
  "https://api.github.com/repos/%s/%s/pulls/%d/comments",
  owner, repo, pr_number
)

local status, _, body = cosmo.Fetch(url, { headers = headers })
if status == 200 then
  local comments = cosmo.DecodeJson(body)
  for i, c in ipairs(comments) do
    print(string.format("%s: %s", c.user.login, c.body))
  end
end
```

## Enabling write access

To enable POST/PATCH operations, set `GITHUB_TOKEN` or `GH_TOKEN`:

```lua
local token = os.getenv("GITHUB_TOKEN")
if token then
  headers["Authorization"] = "Bearer " .. token
end
```

## Git operations

Note: git push/pull work through a separate local proxy at `127.0.0.1:65318` which handles git-specific authentication. This proxy does not support GitHub API requests.
