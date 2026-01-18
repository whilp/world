# appscript: add calendar event colorizer

Google Apps Script to auto-color calendar events based on rules.

## Changes

- `lib/appscript/colorize.gs` - auto-color events based on type:
  - OOO/async events (lavender/sage)
  - Interviews (grape)
  - Focus blocks (blueberry)
  - 1:1s (banana)
  - Group meetings - organizer vs guest (graphite/peacock)
  - Uses People API to detect Google Groups vs individuals
  - Caches group lookups for 6 hours
  - RSVP comment overrides (#ignore, #1:1, #group, #solo)
  - `setup()` - creates hourly trigger and runs initial colorize
- `lib/appscript/colorize.test.js` - tests for rules and event type detection
