# cosmic: add self-bootstrapping bin/cosmic wrapper

Replaces make bootstrap dependency with a minimal bin/cosmic wrapper that downloads cosmic-lua from GitHub releases on first run.

- bin/cosmic - downloads cosmic-lua release and execs it
- .claude/settings.json - SessionStart hook calls BOOTSTRAP=1 bin/cosmic
- .github/workflows/pr.yml - update job uses ./bin/cosmic --skill pr

## Implementation

The bin/cosmic shell script:
1. Downloads cosmic-lua from releases on first run
2. Caches it at bin/cosmic-lua
3. Execs cosmic-lua with all arguments

The SessionStart hook passes BOOTSTRAP=1 to cosmic for initialization. Bootstrap handling will be implemented in cosmic itself in a followup.

## Benefits

- Eliminates make bootstrap dependency for Claude Code sessions
- Simplifies CI setup - no build step needed for PR updates
- Single 17-line shell script handles download and execution
- Uses official cosmic-lua release binary
