---
name: gh-workflow
description: Run GitHub workflows on github.com using GH_HOST environment variable
---

# gh-workflow

Run GitHub workflows on github.com

## Workflow

When the user asks to run a workflow:

1. Check the workflow file in `.github/workflows/` to understand its trigger type
2. For `workflow_dispatch` workflows: trigger directly with `gh workflow run`
3. For `push` workflows: push to the remote first, then the workflow runs automatically
4. Use appropriate parameters based on the workflow's input definitions
5. For iteration/testing: run only the fastest matrix build variant
6. Download workflow artifacts to verify outputs

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
# Run workflow_dispatch workflow with inputs
GH_HOST=github.com gh workflow run luajit.yml -f release_tag=2025.11.23 -f create_release=true

# Run workflow on specific branch
GH_HOST=github.com gh workflow run deploy.yml --ref main

# Run with specific input parameters
GH_HOST=github.com gh workflow run build.yml -f os=ubuntu-latest -f version=3.11

# Download artifacts from latest run
GH_HOST=github.com gh run list --workflow=build.yml --limit 1 --json databaseId --jq '.[0].databaseId' | xargs -I {} GH_HOST=github.com gh run download {}
```
