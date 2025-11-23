---
name: gh-workflow
description: Run GitHub workflows on github.com using GH_HOST environment variable
---

# gh-workflow

Run GitHub workflows on github.com

## Workflow

When the user asks to run a workflow:

1. Check git remotes to find the github.com remote (often named `github`)
2. Push to github.com first: `git push <github-remote> <branch>`
3. Trigger the workflow with appropriate parameters
4. For iteration/testing: run only the fastest matrix build variant
5. Download workflow artifacts to verify outputs

## Usage

```bash
# List workflows
GH_HOST=github.com gh workflow list

# Run a workflow
GH_HOST=github.com gh workflow run <workflow-name>

# Run with inputs
GH_HOST=github.com gh workflow run <workflow-name> -f key=value

# View recent runs
GH_HOST=github.com gh run list --workflow=<workflow-name>

# Watch a run
GH_HOST=github.com gh run watch <run-id>

# Download artifacts from a run
GH_HOST=github.com gh run download <run-id>
```

## Testing matrix builds

For workflows with matrix builds, test with the fastest variant first:
- Prefer smaller/simpler OS (ubuntu over windows/macos)
- Prefer newer language versions
- Use workflow inputs to target specific matrix cells if supported

## Examples

```bash
# Push and run CI workflow
git push github main
GH_HOST=github.com gh workflow run ci.yml

# Run with branch
GH_HOST=github.com gh workflow run deploy.yml --ref main

# Run with matrix targeting (if workflow supports it)
GH_HOST=github.com gh workflow run build.yml -f os=ubuntu-latest -f version=3.11

# Download artifacts from latest run
GH_HOST=github.com gh run list --workflow=build.yml --limit 1 --json databaseId --jq '.[0].databaseId' | xargs -I {} GH_HOST=github.com gh run download {}
```
