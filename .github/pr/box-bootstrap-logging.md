# box: improve bootstrap error visibility

Bootstrap failures were silent - the remote output wasn't shown, and fetch errors lost the actual error message.

## Changes

- `lib/box/sprite.tl` - Pass stdout/stderr through to terminal during exec so bootstrap output is visible
- `lib/box/bootstrap.tl` - Capture and report actual Fetch error (was discarding with `_`)
- `lib/box/bootstrap.tl` - Log download URL for debugging
- `lib/box/bootstrap.tl` - Increase maxresponse from 200MB to 250MB (home binary is 221MB)
