# appscript: add build and test machinery

Add build module for Google Apps Script files, extracted from wcm/calendar-color-rules.

## Changes

- `lib/appscript/cook.mk` - build rules for .gs files and JavaScript tests
- `lib/appscript/run-test.js` - test runner with Google Apps Script mocks
- `lib/appscript/appsscript.json` - manifest with Calendar and People API scopes
- `lib/appscript/.claspignore` - files to exclude from clasp push
- `lib/cook.mk` - include appscript module
- `lib/home/cook.mk` - add .js, .json, .claspignore wildcards to dots_lib
