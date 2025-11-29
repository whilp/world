---
name: cleanshot
description: Take screenshots, screen recordings, scrolling captures, and OCR text recognition using CleanShot X. Use when user requests screenshots, captures, screen recording, OCR, text extraction, or image annotation.
allowed-tools: [Bash]
---

# CleanShot X integration

Trigger CleanShot X actions using its URL scheme API. All commands are executed via `open cleanshot://command`.

## Parameter reference

### action parameter
Specifies post-capture action. Values: `copy`, `save`, `annotate`, `upload`, `pin`

### display parameter
Integer specifying which monitor (1=main, 2=secondary, etc.). If omitted, uses display containing cursor.

### coordinate parameters (x, y, width, height)
Integer values in pixels. Origin (0,0) is at lower left corner of screen.

### boolean parameters (start, autoscroll, linebreaks)
Values: `true` or `false`
- `start` - automatically initiates scrolling capture
- `autoscroll` - enables automatic scrolling functionality
- `linebreaks` - preserves (true) or removes (false) line breaks in OCR text

### filepath parameter
URL-encoded file path to PNG, JPEG, or MP4 files.

### tab parameter
Values: `general`, `wallpaper`, `shortcuts`, `quickaccess`, `recording`, `screenshots`, `annotate`, `cloud`, `advanced`, `about`

## All-in-one mode

**Launch all-in-one capture tool**
```bash
open cleanshot://all-in-one
```

**With specific region and display**
```bash
open "cleanshot://all-in-one?x=100&y=120&width=200&height=150&display=1"
```

Parameters: `x`, `y`, `width`, `height`, `display`

## Screenshot commands

**Area capture**
```bash
open cleanshot://capture-area
```

**Area capture with specific region**
```bash
open "cleanshot://capture-area?x=100&y=120&width=200&height=150"
```

**Area capture with post-action**
```bash
open "cleanshot://capture-area?action=annotate"
```

**Area capture on specific display**
```bash
open "cleanshot://capture-area?display=2"
```

**Area capture saved to Downloads/screens/**
```bash
open "cleanshot://capture-area?action=save&filepath=~/Downloads/screens/screenshot.png"
```

Parameters: `x`, `y`, `width`, `height`, `display`, `action`

**Fullscreen capture**
```bash
open cleanshot://capture-fullscreen
```

**Fullscreen with action**
```bash
open "cleanshot://capture-fullscreen?action=save"
```

**Fullscreen saved to Downloads/screens/**
```bash
open "cleanshot://capture-fullscreen?action=save&filepath=~/Downloads/screens/fullscreen.png"
```

Parameters: `action`

**Window capture**
```bash
open cleanshot://capture-window
```

**Window capture with action**
```bash
open "cleanshot://capture-window?action=copy"
```

Parameters: `action`

**Repeat last screenshot**
```bash
open cleanshot://capture-previous-area
```

**Repeat with different action**
```bash
open "cleanshot://capture-previous-area?action=upload"
```

Parameters: `action`

**Self-timer capture**
```bash
open cleanshot://self-timer
```

**Self-timer with action**
```bash
open "cleanshot://self-timer?action=pin"
```

Parameters: `action`

**Scrolling capture**
```bash
open cleanshot://scrolling-capture
```

**Scrolling capture with region**
```bash
open "cleanshot://scrolling-capture?x=100&y=120&width=200&height=150"
```

**Auto-start scrolling capture**
```bash
open "cleanshot://scrolling-capture?start=true&autoscroll=true"
```

Parameters: `x`, `y`, `width`, `height`, `display`, `start`, `autoscroll`

**Pin image file**
```bash
open "cleanshot://pin?filepath=/Users/john/Desktop/screenshot.png"
```

Parameters: `filepath`

## Screen recording

**Start screen recording**
```bash
open cleanshot://record-screen
```

**Record specific region**
```bash
open "cleanshot://record-screen?x=100&y=120&width=800&height=600"
```

**Record on specific display**
```bash
open "cleanshot://record-screen?display=2"
```

Parameters: `x`, `y`, `width`, `height`, `display`

## Text recognition (OCR)

**Capture text from screen**
```bash
open cleanshot://capture-text
```

**OCR with specific region**
```bash
open "cleanshot://capture-text?x=100&y=120&width=400&height=300"
```

**OCR with line breaks removed**
```bash
open "cleanshot://capture-text?linebreaks=false"
```

**Extract text from file**
```bash
open "cleanshot://capture-text?filepath=/path/to/image.png"
```

**OCR from file without line breaks**
```bash
open "cleanshot://capture-text?filepath=/path/to/image.png&linebreaks=false"
```

Parameters: `filepath`, `x`, `y`, `width`, `height`, `display`, `linebreaks`

## Annotation

**Annotate image file**
```bash
open "cleanshot://open-annotate?filepath=/path/to/image.png"
```

Parameters: `filepath`

**Annotate clipboard image**
```bash
open cleanshot://open-from-clipboard
```

## Desktop management

**Toggle desktop icons**
```bash
open cleanshot://toggle-desktop-icons
```

**Hide desktop icons**
```bash
open cleanshot://hide-desktop-icons
```

**Show desktop icons**
```bash
open cleanshot://show-desktop-icons
```

## Quick access overlay

**Add quick access overlay**
```bash
open "cleanshot://add-quick-access-overlay?filepath=/path/to/file.png"
```

Parameters: `filepath` (supports PNG, JPEG, MP4)

## History

**Open capture history**
```bash
open cleanshot://open-history
```

**Restore recently deleted**
```bash
open cleanshot://restore-recently-closed
```

## Settings

**Open settings**
```bash
open cleanshot://open-settings
```

**Open specific settings tab**
```bash
open "cleanshot://open-settings?tab=shortcuts"
```

Parameters: `tab` (values: `general`, `wallpaper`, `shortcuts`, `quickaccess`, `recording`, `screenshots`, `annotate`, `cloud`, `advanced`, `about`)

## Usage examples

When user requests:
- "take a screenshot" → use `capture-area` or `capture-fullscreen`
- "save a screenshot" → use `capture-area?action=save&filepath=~/Downloads/screens/screenshot.png`
- "record my screen" → use `record-screen`
- "extract text from image" → use `capture-text`
- "annotate this image" → use `open-annotate`
- "hide desktop icons" → use `hide-desktop-icons`
- "scrolling screenshot" → use `scrolling-capture`
- "capture this specific region" → use coordinate parameters with appropriate command

Always confirm which capture type the user wants if ambiguous.

**Note:** When saving screenshots, always use `~/Downloads/screens/` as the target directory.
