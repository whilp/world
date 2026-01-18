# appscript: add calendar color rules with clasp testing

Add Google Apps Script for automatic calendar event color coding, along with build
integration and automated testing using clasp.

## Changes

### Calendar color rules (colorize.gs)
- `lib/appscript/colorize.gs` - Auto-color calendar events based on rules:
  - OOO/async events (lavender/sage)
  - Interviews (grape)
  - Focus blocks (blueberry)
  - 1:1s (banana)
  - Group meetings - organizer vs guest (graphite/peacock)
- Uses People API to detect Google Groups vs individuals
- Caches group lookups for 6 hours
- RSVP comment overrides (#ignore, #1:1, #group, #solo)

### Auto-accept (auto-accept.gs)
- `lib/appscript/auto-accept.gs` - Auto-accept invites from a configured calendar

### Busy blocks (busy.gs)
- `lib/appscript/busy.gs` - Fill calendar gaps with "Busy" blocks

### Build integration
- `lib/appscript/cook.mk` - Build module for appscript
- `lib/appscript/appsscript.json` - Apps Script manifest with People and Calendar APIs
- `lib/appscript/.clasp.json.example` - Template for clasp project config
- `lib/appscript/.claspignore` - Files to exclude from push
- `lib/cook.mk` - Include appscript module

### Testing
- `lib/appscript/run-tests.js` - JavaScript test runner with mocked Google APIs
- `lib/appscript/test_appscript.tl` - Teal wrapper that invokes the JS tests
- Tests cover:
  - busy.gs: working hours, day detection, slot rounding, gap finding
  - colorize.gs: all color rules, event type detection, attendee analysis
  - auto-accept.gs: calendar property storage
