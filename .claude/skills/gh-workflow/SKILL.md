---
name: gh-workflow
description: Trigger and monitor GitHub Actions workflows, CI/CD pipelines, and automated builds. Use when running workflows, triggering CI, deploying via GitHub Actions, downloading artifacts, or checking workflow status on github.com
---

# gh-workflow

Run GitHub workflows on github.com

## Workflow

When the user asks to run a workflow:

1. Check the workflow file in `.github/workflows/` to understand its trigger type and inputs
2. **Ensure code is pushed to the github.com remote** before triggering workflows:
   - Workflows run on github.com, not corp/internal git servers
   - Check the correct remote with `git remote -v | grep github.com`
   - Push to the github remote: `git push github main` (or appropriate remote name)
   - Verify the push completed: `git fetch github && git log github/main -1`
3. For `workflow_dispatch` workflows: trigger directly with `gh workflow run`
4. For `push` workflows: push to the remote first, then the workflow runs automatically
5. Use appropriate parameters based on the workflow's input definitions
6. For testing/iteration with matrix builds: prefer x64 over arm64 (faster provisioning)
7. Download workflow artifacts to verify outputs

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
- Prefer x64 over arm64 (x64 runners provision faster)
- Prefer ubuntu over macos (faster startup)
- Use workflow inputs to target specific matrix cells if supported

## Examples

```bash
# Run workflow_dispatch workflow with inputs
GH_HOST=github.com gh workflow run luajit.yml -f release_tag=2025.11.23 -f create_release=true

# Run workflow on specific branch
GH_HOST=github.com gh workflow run luajit.yml --ref main

# Run without creating release (testing)
GH_HOST=github.com gh workflow run nvim.yml -f create_release=false

# Download artifacts from latest run
GH_HOST=github.com gh run list --workflow=luajit.yml --limit 1 --json databaseId --jq '.[0].databaseId' | xargs -I {} GH_HOST=github.com gh run download {}
```
