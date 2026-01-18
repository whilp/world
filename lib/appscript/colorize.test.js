// Tests for colorize.gs - calendar event color coding

const colorize = loadGsFile("colorize.gs");

log("colorize.gs:");

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
