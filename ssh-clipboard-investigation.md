# SSH Clipboard Integration with Alacritty Window ID

## Goal
Map Alacritty window ID to the relevant SSH ControlPath socket, enabling an Alacritty keybinding to invoke a program that copies clipboard contents over the correct SSH connection.

## What We've Learned

### Alacritty Environment Variables
- `$ALACRITTY_WINDOW_ID`: Unique identifier per window (e.g., `4982853824`)
- `$ALACRITTY_SOCKET`: Per-instance socket (e.g., `/var/folders/.../Alacritty-83851.sock`)

### SSH ControlPath Configuration
- SSH config supports environment variables in `ControlPath` using `${VARIABLE}` syntax
- Added to `~/.ssh/personal.config`:
  ```
  Host *
    ControlMaster auto
    ControlPath ~/.ssh/sockets/%r@%h-%p-window-${ALACRITTY_WINDOW_ID}
    ControlPersist 10m
  ```

### Pay Command Behavior
- `pay remote ssh wcm-dots` bypasses our SSH config
- Creates its own socket at `/tmp/paycmd/qa-mydev--036137daed7b80ab8.northwest.stripe.io.sock`
- Uses Ruby wrapper that manages SSH connections independently
- Process tree shows: `ssh: /tmp/paycmd/qa-mydev--036137daed7b80ab8.northwest.stripe.io.sock [mux]`

### What Didn't Work
1. **SSH Config ControlPath**: `pay remote ssh` ignores our `~/.ssh/config` ControlPath settings
2. **Environment Variable Expansion**: Tried `%{ALACRITTY_WINDOW_ID}`, `$ALACRITTY_WINDOW_ID`, `${ALACRITTY_WINDOW_ID}` - pay command bypasses all
3. **Direct SSH**: Raw SSH connections to devbox hosts fail due to network configuration

## Current State
- Pay command creates sockets in `/tmp/paycmd/` with format `hostname.sock`
- Our SSH config creates directory `~/.ssh/sockets/` but no sockets are created there
- Window ID (`4982853824`) is available but not integrated into socket naming

## Next Steps / Alternative Approaches

### Option 1: Hook into Pay Command Sockets
- Monitor `/tmp/paycmd/` for existing sockets
- Map window ID to active pay command processes
- Use existing sockets for clipboard transfer

### Option 2: Custom SSH Wrapper
- Create wrapper script that uses window ID in socket path
- Override SSH behavior to use our ControlPath
- May require bypassing pay command entirely

### Option 3: Process-Based Detection
- Parse process tree to find SSH connections per window
- Extract target host from running SSH processes
- Create new connections using our ControlPath configuration

### Option 4: Window-to-Target Mapping
- Cache mapping of window ID to SSH target
- Update cache when SSH connections are established
- Use cached mappings for clipboard operations

## Key Files Modified
- `~/.ssh/personal.config`: Added ControlPath with window ID
- `~/.ssh/sockets/`: Created directory (unused due to pay command behavior)

## Commands Tested
- `pay remote ssh wcm-dots -- echo "test"` - Works but uses own sockets
- Direct SSH to `qa-mydev--036137daed7b80ab8.northwest.stripe.io` - Connection refused
- `/usr/bin/ssh -F ~/.ssh/config` - Config ignored by pay wrapper
- SSH `LocalCommand` with `PermitLocalCommand yes` - Not executed by pay command

## LocalCommand Investigation
- Added `LocalCommand ~/.local/bin/ssh-local-command` to SSH config
- Created script to log environment variables and execution
- Pay command completely bypasses SSH config, including LocalCommand
- No `/tmp/ssh-local-command.log` file created despite multiple connection attempts

## Working Solution: Process-Based Detection
Since the pay command bypasses SSH config, we need to:
1. Detect existing SSH sockets in `/tmp/paycmd/`
2. Map window ID to active SSH connections via process inspection
3. Use existing sockets for clipboard operations

## Current Active SSH Processes
```
wcm    89115  ssh: /tmp/paycmd/qa-mydev--036137daed7b80ab8.northwest.stripe.io.sock [mux]
wcm    89109  /usr/bin/ssh -W [qa-mydev--036137daed7b80ab8.northwest.stripe.io]:30000 sshbastion-qa-peered.corp.stripe.com
```