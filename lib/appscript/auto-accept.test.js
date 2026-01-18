// Tests for auto-accept.gs - auto-accept calendar invites

const autoAccept = loadGsFile("auto-accept.gs");

log("auto-accept.gs:");

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
