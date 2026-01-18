#!/usr/bin/env bun
// Test runner for appscript - outputs in format compatible with test reporter
// Output format:
//   pass (or fail: message)
//   ## stdout
//   <stdout>
//   ## stderr
//   <stderr>

import { readFileSync } from "fs";
import { dirname, join } from "path";

const srcDir = dirname(import.meta.path);
let failures = [];
let stdout = [];

function log(msg) {
  stdout.push(msg);
}

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function assertEqual(actual, expected, message) {
  if (actual !== expected) {
    throw new Error(`${message}: expected ${expected}, got ${actual}`);
  }
}

// Setup mocks for Google Apps Script globals
globalThis.CalendarApp = {
  getDefaultCalendar: () => ({
    getEvents: () => [],
    createEvent: () => ({}),
  }),
};

globalThis.ScriptApp = {
  getProjectTriggers: () => [],
  deleteTrigger: () => {},
  newTrigger: () => ({
    timeBased: () => ({
      everyHours: () => ({ create: () => {} }),
    }),
    forUserCalendar: () => ({
      onEventUpdated: () => ({ create: () => {} }),
    }),
  }),
};

globalThis.Logger = { log: () => {} };
globalThis.Utilities = { sleep: () => {} };

globalThis.CacheService = {
  getScriptCache: () => ({
    get: () => null,
    put: () => {},
  }),
};

globalThis.People = {
  People: {
    searchDirectoryPeople: (opts) => {
      // Mock: return the queried email as a found person (so it's not a group)
      const email = opts.query;
      if (email && !email.includes("@resource.calendar.google.com")) {
        return {
          people: [{
            emailAddresses: [{ value: email }]
          }]
        };
      }
      return { people: [] };
    },
  },
};

globalThis.Calendar = {
  Events: {
    get: () => ({ attendees: [] }),
  },
};

globalThis.Session = {
  getActiveUser: () => ({
    getEmail: () => "test@example.com",
  }),
};

const mockProperties = {};
globalThis.PropertiesService = {
  getScriptProperties: () => ({
    getProperty: (key) => mockProperties[key] || null,
    setProperty: (key, value) => { mockProperties[key] = value; },
  }),
};

// Load a .gs file and return its exports
function loadGsFile(filename) {
  const code = readFileSync(join(srcDir, filename), "utf-8");
  // Extract top-level function names (at start of line or after semicolon/brace)
  const functionNames = [...code.matchAll(/(?:^|\n)function\s+(\w+)\s*\(/g)].map(m => m[1]);
  // Extract top-level const names (at start of line)
  const constNames = [...code.matchAll(/(?:^|\n)const\s+(\w+)\s*=/g)].map(m => m[1]);
  const allNames = [...functionNames, ...constNames];

  const wrappedCode = code + `\nreturn { ${allNames.join(", ")} };`;
  return new Function(wrappedCode)();
}

// Test helper
function runTest(name, fn) {
  try {
    fn();
    log(`  ✓ ${name}`);
  } catch (e) {
    failures.push(`${name}: ${e.message}`);
    log(`  ✗ ${name}: ${e.message}`);
  }
}

// ============= busy.gs tests =============
log("busy.gs:");
const busy = loadGsFile("busy.gs");

runTest("getWorkingHours returns defaults", () => {
  const h = busy.getWorkingHours();
  assertEqual(h.startHour, 9, "startHour");
  assertEqual(h.endHour, 17, "endHour");
});

runTest("isWorkingDay: monday is working day", () => {
  assert(busy.isWorkingDay(new Date("2024-01-08")), "monday should be working day");
});

runTest("isWorkingDay: saturday is not working day", () => {
  assert(!busy.isWorkingDay(new Date("2024-01-13")), "saturday should not be working day");
});

runTest("getWeekStart returns sunday at midnight", () => {
  const wed = new Date("2024-01-10T15:30:00");
  const start = busy.getWeekStart(wed);
  assertEqual(start.getDay(), 0, "day");
  assertEqual(start.getHours(), 0, "hours");
});

runTest("roundUpToSlot: 0 stays at 0", () => {
  const d = new Date("2024-01-10T10:00:00");
  const r = busy.roundUpToSlot(d);
  assertEqual(r.getMinutes(), 0, "minutes");
});

runTest("roundUpToSlot: 15 rounds to 30", () => {
  const d = new Date("2024-01-10T10:15:00");
  const r = busy.roundUpToSlot(d);
  assertEqual(r.getMinutes(), 30, "minutes");
});

runTest("roundUpToSlot: 45 rounds to next hour", () => {
  const d = new Date("2024-01-10T10:45:00");
  const r = busy.roundUpToSlot(d);
  assertEqual(r.getMinutes(), 0, "minutes");
  assertEqual(r.getHours(), 11, "hours");
});

runTest("roundDownToSlot: 15 rounds to 0", () => {
  const d = new Date("2024-01-10T10:15:00");
  const r = busy.roundDownToSlot(d);
  assertEqual(r.getMinutes(), 0, "minutes");
});

runTest("roundDownToSlot: 45 rounds to 30", () => {
  const d = new Date("2024-01-10T10:45:00");
  const r = busy.roundDownToSlot(d);
  assertEqual(r.getMinutes(), 30, "minutes");
});

runTest("findGaps: no events returns full day", () => {
  const start = new Date("2024-01-10T09:00:00");
  const end = new Date("2024-01-10T17:00:00");
  const gaps = busy.findGaps([], start, end);
  assertEqual(gaps.length, 1, "gap count");
});

runTest("findGaps: one event creates two gaps", () => {
  const start = new Date("2024-01-10T09:00:00");
  const end = new Date("2024-01-10T17:00:00");
  const events = [{
    getStartTime: () => new Date("2024-01-10T12:00:00"),
    getEndTime: () => new Date("2024-01-10T13:00:00"),
  }];
  const gaps = busy.findGaps(events, start, end);
  assertEqual(gaps.length, 2, "gap count");
});

// ============= colorize.gs tests =============
log("\ncolorize.gs:");
const colorize = loadGsFile("colorize.gs");

runTest("RULES has expected count", () => {
  assert(colorize.RULES.length >= 7, "should have at least 7 rules");
});

runTest("out-of-office rule matches OOO", () => {
  const rule = colorize.RULES.find(r => r.name === "out-of-office");
  assert(rule.match({ getTitle: () => "OOO" }), "should match OOO");
});

runTest("out-of-office rule matches 'Out of Office'", () => {
  const rule = colorize.RULES.find(r => r.name === "out-of-office");
  assert(rule.match({ getTitle: () => "Out of Office" }), "should match Out of Office");
});

runTest("async rule matches [async]", () => {
  const rule = colorize.RULES.find(r => r.name === "async");
  assert(rule.match({ getTitle: () => "[async] Review" }), "should match [async]");
});

runTest("interview rule matches interview", () => {
  const rule = colorize.RULES.find(r => r.name === "interview");
  assert(rule.match({ getTitle: () => "Technical Interview" }), "should match interview");
});

runTest("focus-block matches Busy with no guests", () => {
  const rule = colorize.RULES.find(r => r.name === "focus-block");
  const event = { getTitle: () => "Busy", getGuestList: () => [] };
  assert(rule.match(event), "should match Busy with no guests");
});

runTest("focus-block does not match Busy with guests", () => {
  const rule = colorize.RULES.find(r => r.name === "focus-block");
  const event = { getTitle: () => "Busy", getGuestList: () => [{}] };
  assert(!rule.match(event), "should not match Busy with guests");
});

runTest("isGroup: resource calendars are not groups", () => {
  assert(!colorize.isGroup("room@resource.calendar.google.com"), "resource should not be group");
});

runTest("getEventType: solo with no attendees", () => {
  const event = {
    getGuestList: () => [],
    getCreators: () => ["test@example.com"],
  };
  assertEqual(colorize.getEventType(event), "solo", "type");
});

runTest("getEventType: 1:1 with one other person", () => {
  const event = {
    getGuestList: () => [{ getEmail: () => "other@example.com" }],
    getCreators: () => ["test@example.com"],
  };
  assertEqual(colorize.getEventType(event), "1:1", "type");
});

runTest("getEventType: group with 2+ others", () => {
  const event = {
    getGuestList: () => [
      { getEmail: () => "a@example.com" },
      { getEmail: () => "b@example.com" },
    ],
    getCreators: () => ["test@example.com"],
  };
  assertEqual(colorize.getEventType(event), "group", "type");
});

// ============= auto-accept.gs tests =============
log("\nauto-accept.gs:");
const autoAccept = loadGsFile("auto-accept.gs");

runTest("getOtherCalendar returns null when not set", () => {
  delete mockProperties["other_calendar_email"];
  assertEqual(autoAccept.getOtherCalendar(), null, "should be null");
});

runTest("setOtherCalendar stores email", () => {
  autoAccept.setOtherCalendar("other@example.com");
  assertEqual(mockProperties["other_calendar_email"], "other@example.com", "stored value");
});

runTest("getOtherCalendar returns stored value", () => {
  mockProperties["other_calendar_email"] = "stored@example.com";
  assertEqual(autoAccept.getOtherCalendar(), "stored@example.com", "retrieved value");
});

// Output results
console.log(failures.length === 0 ? "pass" : `fail: ${failures.length} test(s) failed`);
console.log("");
console.log("## stdout");
console.log("");
console.log(stdout.join("\n"));
console.log("## stderr");
console.log("");

process.exit(failures.length === 0 ? 0 : 1);
