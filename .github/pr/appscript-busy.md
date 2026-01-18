# appscript: add busy calendar filler

Google Apps Script to fill calendar gaps with "Busy" blocks.

## Changes

- `lib/appscript/busy.gs` - fill calendar gaps with busy events:
  - Creates 25 or 50 minute "Busy" blocks in free time slots
  - Works within configurable working hours (default 9-5, M-F)
  - Current week: fills all gaps
  - Next week: leaves 2 hours open
  - Later weeks: leaves 4 hours open
  - `setupBusyFill()` - creates 4-hour trigger and runs initial fill
- `lib/appscript/busy.test.js` - tests for time utilities and gap finding
