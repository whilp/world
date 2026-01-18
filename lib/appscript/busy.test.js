// Tests for busy.gs - calendar busy block filling

const busy = loadGsFile("busy.gs");

log("busy.gs:");

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
