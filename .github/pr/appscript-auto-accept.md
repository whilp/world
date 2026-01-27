# appscript: add auto-accept calendar invites

Google Apps Script to automatically accept calendar invites from a configured email address.

## Changes

- `lib/appscript/auto-accept.gs` - auto-accept invites from a configured calendar:
  - `setOtherCalendar(email)` - configure which sender to auto-accept
  - `setupAutoAccept()` - create hourly and calendar-change triggers
  - Auto-accepts INVITED/MAYBE events from the configured organizer
- `lib/appscript/auto-accept.test.js` - tests for property storage
- `lib/appscript/.claspignore` - add test files to ignore list
