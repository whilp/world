# box: add self-bootstrapping remote environment manager

Adds a new `box` command that creates, bootstraps, and connects to remote
development environments with a single command. The box binary is a
cosmopolitan executable that:

1. Bundles zip/unzip tools for self-modification
2. At runtime, zips env.lua into a copy of itself
3. Uploads the modified binary to the remote
4. Runs bootstrap with credentials embedded in /zip/env.lua

## CLI

```bash
# All-in-one (create + bootstrap + ssh)
box --sprite dev

# Individual commands
box --sprite dev new    # create only
box --sprite dev run    # bootstrap only
box --sprite dev ssh    # connect only
box --sprite dev zap    # destroy

# Custom backend
box --backend pay.lua dev

# Options
box --env path/to/env.lua   # credentials (default: ~/.config/box/env.lua)
```

## env.lua format

Can be a table or a function that takes the box name:

```lua
-- Simple table form
return {
  github = {
    ["github.com"] = "ghp_...",
    ["git.corp.example.com"] = "ghp_enterprise_...",
  },
  claude = {
    token = "sk-ant-oat01-...",  -- written to ~/.env
    credentials = {              -- optional: encoded to JSON for ~/.claude/.credentials.json
      key = "value",
    },
  },
}

-- Function form (receives box name)
return function(name)
  return {
    github = { ["github.com"] = "ghp_..." },
  }
end
```

## Changes

- `lib/box/backend.tl` - type definitions (Result, Backend, Env)
- `lib/box/bootstrap.tl` - downloads and unpacks home from releases
- `lib/box/init.tl` - main CLI entry point, self-modifying zip logic
- `lib/box/run.tl` - bootstrap logic (runs on remote)
- `lib/box/sprite.tl` - sprites.dev backend
- `lib/box/mac.tl` - local mac backend (placeholder)
- `lib/box/cook.mk` - build rules (bundles cosmos zip/unzip)
- `lib/cook.mk` - include box module

Binary size: ~7.3MB (includes bundled zip ~1.8MB, unzip ~1.2MB)

## Validated

Tested on sprites.dev:
- Creates sprite, zips env.lua into box copy, uploads
- Runs bootstrap with /zip/env.lua credentials
- Downloads and unpacks home bundle
- nvim, gh, and other tools work
