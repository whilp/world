#!/usr/bin/env bun
// Test runner for appscript - runs a single test file
// Usage: bun run run-test.js <test-file.test.js>
// Output format (compatible with test reporter):
//   pass (or fail: message)
//   ## stdout
//   <stdout>
//   ## stderr
//   <stderr>

import { dirname, join, resolve } from "path";

const srcDir = dirname(import.meta.path);
const testFileArg = process.argv[2];
const testFile = testFileArg ? resolve(testFileArg) : null;

if (!testFile) {
  console.log("fail: no test file specified");
  console.log("");
  console.log("## stdout");
  console.log("");
  console.log("usage: run-test.js <test-file.test.js>");
  console.log("## stderr");
  console.log("");
  process.exit(0);
}

// Test state
let failures = [];
let stdout = [];

// Export test utilities to global scope
globalThis.TEST_SRCDIR = srcDir;
globalThis.TEST_FAILURES = failures;
globalThis.TEST_STDOUT = stdout;

globalThis.log = function(msg) {
  stdout.push(msg);
};

globalThis.assert = function(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
};

globalThis.assertEqual = function(actual, expected, message) {
  if (actual !== expected) {
    throw new Error(`${message}: expected ${expected}, got ${actual}`);
  }
};

globalThis.runTest = function(name, fn) {
  try {
    fn();
    log(`  ✓ ${name}`);
  } catch (e) {
    failures.push(`${name}: ${e.message}`);
    log(`  ✗ ${name}: ${e.message}`);
  }
};

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

globalThis.mockProperties = {};
globalThis.PropertiesService = {
  getScriptProperties: () => ({
    getProperty: (key) => mockProperties[key] || null,
    setProperty: (key, value) => { mockProperties[key] = value; },
  }),
};

// Helper to load a .gs file and return its exports
import { readFileSync } from "fs";

globalThis.loadGsFile = function(filename) {
  const code = readFileSync(join(srcDir, filename), "utf-8");
  return new Function(code)();
};

// Run the test file
try {
  await import(testFile);
} catch (e) {
  failures.push(`import error: ${e.message}`);
}

// Output results
console.log(failures.length === 0 ? "pass" : `fail: ${failures.length} test(s) failed`);
console.log("");
console.log("## stdout");
console.log("");
console.log(stdout.join("\n"));
console.log("## stderr");
console.log("");

// Always exit 0 - reporter determines pass/fail from output
process.exit(0);
