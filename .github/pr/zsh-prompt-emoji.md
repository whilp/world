# zsh: use whereami emoji in prompt

Replaces the simple `%#` prompt with the emoji from the WHEREAMI variable.

## Changes

- `.zshrc` - change PS1 to extract and display emoji from WHEREAMI using `${WHEREAMI##* }`
