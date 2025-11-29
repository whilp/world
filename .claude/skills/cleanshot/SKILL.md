---
name: cleanshot
description: Take screenshots, screen recordings, scrolling captures, and OCR text recognition using CleanShot X. Use when user requests screenshots, captures, screen recording, OCR, text extraction, or image annotation.
allowed-tools: [Bash]
---

# CleanShot X integration

Trigger CleanShot X actions using its URL scheme API. All commands are executed via `open cleanshot://command`.

## Screenshot commands

**Area capture**
```bash
open cleanshot://capture-area
```

**Fullscreen capture**
```bash
open cleanshot://capture-fullscreen
```

**Window capture**
```bash
open cleanshot://capture-window
```

**Repeat last screenshot**
```bash
open cleanshot://capture-previous-area
```

**Self-timer capture**
```bash
open "cleanshot://self-timer?delay=3"
```

**Scrolling capture**
```bash
open cleanshot://scrolling-capture
```

## Screen recording

**Start screen recording**
```bash
open cleanshot://record-screen
```

## Text recognition (OCR)

**Capture text from screen**
```bash
open cleanshot://capture-text
```

**Extract text from file**
```bash
open "cleanshot://capture-text?filepath=/path/to/image.png"
```

## Annotation

**Annotate image file**
```bash
open "cleanshot://open-annotate?filepath=/path/to/image.png"
```

**Annotate clipboard image**
```bash
open cleanshot://open-from-clipboard
```

## Post-capture actions

Add `action` parameter to specify what happens after capture:

- `action=copy` - Copy to clipboard
- `action=save` - Save to disk
- `action=annotate` - Open in annotation editor
- `action=upload` - Upload to cloud
- `action=pin` - Create floating pin

Example:
```bash
open "cleanshot://capture-area?action=annotate"
```

### Save location

Screenshots should be saved to `~/Downloads/screens/`. When using the `action=save` parameter or when saving screenshots with a filepath, use this directory:

```bash
# Save area capture to Downloads/screens/
open "cleanshot://capture-area?action=save&filepath=~/Downloads/screens/screenshot.png"
```

```bash
# Save fullscreen capture to Downloads/screens/
open "cleanshot://capture-fullscreen?action=save&filepath=~/Downloads/screens/fullscreen.png"
```

## Desktop management

**Hide desktop icons**
```bash
open cleanshot://hide-desktop-icons
```

**Show desktop icons**
```bash
open cleanshot://show-desktop-icons
```

**Toggle desktop icons**
```bash
open cleanshot://toggle-desktop-icons
```

## Additional tools

**All-in-one interface**
```bash
open cleanshot://all-in-one
```

**Open capture history**
```bash
open cleanshot://open-history
```

**Restore recently deleted**
```bash
open cleanshot://restore-recently-closed
```

## Usage examples

When user requests:
- "take a screenshot" → use `capture-area` or `capture-fullscreen`
- "save a screenshot" → use `capture-area?action=save&filepath=~/Downloads/screens/screenshot.png`
- "record my screen" → use `record-screen`
- "extract text from image" → use `capture-text`
- "annotate this image" → use `open-annotate`
- "hide desktop icons" → use `hide-desktop-icons`
- "scrolling screenshot" → use `scrolling-capture`

Always confirm which capture type the user wants if ambiguous.

**Note:** When saving screenshots, always use `~/Downloads/screens/` as the target directory.
