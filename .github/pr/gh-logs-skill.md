# gh-logs: add script to fetch GitHub action logs

Adds a new cosmic skill to fetch and display GitHub Actions job status using the REST API.

## Changes

- `lib/skill/gh-logs.tl` - new skill that queries GitHub Actions API

## Features

- Works without authentication for public repos (uses public REST API endpoints)
- Auto-detects PR for current branch when no URL provided
- Supports multiple URL formats: PR, run, or job URLs
- Shows job status, step timing, and failure annotations
- Optional `--logs` flag to download full logs (requires GITHUB_TOKEN)
- Uses `cosmo.getopt` for argument parsing
