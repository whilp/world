---
name: gh-workflow
description: Run GitHub workflows on github.com using GH_HOST environment variable
---

# gh-workflow

Run GitHub workflows on github.com

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
```

## Examples

```bash
# Run CI workflow
GH_HOST=github.com gh workflow run ci.yml

# Run with branch
GH_HOST=github.com gh workflow run deploy.yml --ref main

# Run with multiple inputs
GH_HOST=github.com gh workflow run release.yml -f version=1.0.0 -f environment=prod
```
