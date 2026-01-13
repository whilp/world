# checker: return 0 on failures to allow summary generation

Previously checkers (teal, luacheck, ast-grep) returned exit code 1 on failures, causing Make to stop before running the reporter. This meant no summary was generated when checks failed.

Now checkers always return 0 (like tests do), while still writing "fail" status to output files. This allows Make to continue to the reporter step, which generates summaries even when there are failures.

- lib/checker/common.lua - changed write_result to always return 0

## Validation

- [ ] tests pass
- [ ] checkers pass
