# skill/bootstrap: provide cosmic context to agents

Updates the bootstrap skill (invoked via SessionStart hook) to provide helpful context about the cosmic runtime and its key modules.

Changes:
- lib/skill/bootstrap.lua - adds 185-word context message covering cosmic modules, help system, skills, and constraints

The context tells agents:
- What cosmic is (cosmopolitan lua with bundled libraries)
- How to get help (`cosmic --help`, reading source)
- Key modules: cosmo (HTTP, JSON, crypto), cosmo.path (paths), cosmo.unix (POSIX API), cosmic.spawn (processes)
- Available skills (especially `cosmic --skill pr` for PR management)
- Critical constraints (use path.join, use spawn not os.execute, no octal literals)

This provides agents with enough information to know what to look for later without overwhelming the SessionStart context. The message is concise but includes pointers to documentation and common pitfalls.

## Validation

- [x] tests pass
- [x] bootstrap skill outputs context correctly
- [x] word count appropriate for SessionStart hook (185 words)
