// Tests for auto-accept.gs - auto-accept calendar invites

const autoAccept = loadGsFile("auto-accept.gs");

log("auto-accept.gs:");

// Property storage tests
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

// autoAcceptOtherCalendarInvites tests
runTest("autoAccept returns early when no email configured", () => {
  delete mockProperties["other_calendar_email"];
  mockCalendarEvents = [];
  const result = autoAccept.autoAcceptOtherCalendarInvites();
  assertEqual(result, undefined, "should return undefined");
});

runTest("autoAccept returns 0 when no events", () => {
  mockProperties["other_calendar_email"] = "organizer@example.com";
  mockCalendarEvents = [];
  const result = autoAccept.autoAcceptOtherCalendarInvites();
  assertEqual(result, 0, "should return 0");
});

runTest("autoAccept accepts INVITED event from configured organizer", () => {
  mockProperties["other_calendar_email"] = "organizer@example.com";
  const event = createMockEvent({
    organizer: "organizer@example.com",
    status: CalendarApp.GuestStatus.INVITED,
    title: "Test Meeting",
  });
  mockCalendarEvents = [event];

  const result = autoAccept.autoAcceptOtherCalendarInvites();
  assertEqual(result, 1, "should accept 1 event");
  assertEqual(event._getSetStatus(), CalendarApp.GuestStatus.YES, "status should be YES");
});

runTest("autoAccept accepts MAYBE event from configured organizer", () => {
  mockProperties["other_calendar_email"] = "organizer@example.com";
  const event = createMockEvent({
    organizer: "organizer@example.com",
    status: CalendarApp.GuestStatus.MAYBE,
  });
  mockCalendarEvents = [event];

  const result = autoAccept.autoAcceptOtherCalendarInvites();
  assertEqual(result, 1, "should accept 1 event");
  assertEqual(event._getSetStatus(), CalendarApp.GuestStatus.YES, "status should be YES");
});

runTest("autoAccept ignores already accepted events", () => {
  mockProperties["other_calendar_email"] = "organizer@example.com";
  const event = createMockEvent({
    organizer: "organizer@example.com",
    status: CalendarApp.GuestStatus.YES,
  });
  mockCalendarEvents = [event];

  const result = autoAccept.autoAcceptOtherCalendarInvites();
  assertEqual(result, 0, "should accept 0 events");
});

runTest("autoAccept ignores events from other organizers", () => {
  mockProperties["other_calendar_email"] = "organizer@example.com";
  const event = createMockEvent({
    organizer: "someone-else@example.com",
    status: CalendarApp.GuestStatus.INVITED,
  });
  mockCalendarEvents = [event];

  const result = autoAccept.autoAcceptOtherCalendarInvites();
  assertEqual(result, 0, "should accept 0 events");
  assertEqual(event._getSetStatus(), CalendarApp.GuestStatus.INVITED, "status unchanged");
});

runTest("autoAccept matches organizer case-insensitively", () => {
  mockProperties["other_calendar_email"] = "Organizer@Example.com";
  const event = createMockEvent({
    organizer: "organizer@example.com",
    status: CalendarApp.GuestStatus.INVITED,
  });
  mockCalendarEvents = [event];

  const result = autoAccept.autoAcceptOtherCalendarInvites();
  assertEqual(result, 1, "should accept 1 event");
});

runTest("autoAccept handles events with no organizer", () => {
  mockProperties["other_calendar_email"] = "organizer@example.com";
  const event = createMockEvent({
    status: CalendarApp.GuestStatus.INVITED,
  });
  mockCalendarEvents = [event];

  const result = autoAccept.autoAcceptOtherCalendarInvites();
  assertEqual(result, 0, "should accept 0 events");
});

runTest("autoAccept processes multiple events correctly", () => {
  mockProperties["other_calendar_email"] = "organizer@example.com";
  const event1 = createMockEvent({
    organizer: "organizer@example.com",
    status: CalendarApp.GuestStatus.INVITED,
  });
  const event2 = createMockEvent({
    organizer: "organizer@example.com",
    status: CalendarApp.GuestStatus.MAYBE,
  });
  const event3 = createMockEvent({
    organizer: "other@example.com",
    status: CalendarApp.GuestStatus.INVITED,
  });
  const event4 = createMockEvent({
    organizer: "organizer@example.com",
    status: CalendarApp.GuestStatus.YES,
  });
  mockCalendarEvents = [event1, event2, event3, event4];

  const result = autoAccept.autoAcceptOtherCalendarInvites();
  assertEqual(result, 2, "should accept 2 events");
  assertEqual(event1._getSetStatus(), CalendarApp.GuestStatus.YES, "event1 accepted");
  assertEqual(event2._getSetStatus(), CalendarApp.GuestStatus.YES, "event2 accepted");
  assertEqual(event3._getSetStatus(), CalendarApp.GuestStatus.INVITED, "event3 unchanged");
  assertEqual(event4._getSetStatus(), CalendarApp.GuestStatus.YES, "event4 unchanged");
});

// setupAutoAccept tests
runTest("setupAutoAccept returns early when no email configured", () => {
  delete mockProperties["other_calendar_email"];
  // Should not throw
  autoAccept.setupAutoAccept();
});
