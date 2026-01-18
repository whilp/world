const BUSY_TITLE = 'Busy';
const DEFAULT_WORK_START_HOUR = 9;
const DEFAULT_WORK_END_HOUR = 17;
const NEXT_WEEK_OPEN_MINUTES = 120; // 2 hours
const LATER_WEEK_OPEN_MINUTES = 240; // 4 hours

function getWorkingHours() {
  return {
    startHour: DEFAULT_WORK_START_HOUR,
    endHour: DEFAULT_WORK_END_HOUR
  };
}

function isWorkingDay(date) {
  const day = date.getDay();
  return day >= 1 && day <= 5;
}

function getWeekStart(date) {
  const d = new Date(date);
  d.setDate(d.getDate() - d.getDay());
  d.setHours(0, 0, 0, 0);
  return d;
}

function getDateRange() {
  const now = new Date();
  const today = new Date(now.getFullYear(), now.getMonth(), now.getDate());
  const weekStart = getWeekStart(today);

  // Current week + 3 more weeks
  const endDate = new Date(weekStart);
  endDate.setDate(weekStart.getDate() + 28);

  return { start: weekStart, end: endDate };
}

function deleteExistingBusyEvents(calendar, startDate, endDate) {
  const events = calendar.getEvents(startDate, endDate);
  let deleted = 0;

  for (const event of events) {
    if (event.getTitle() === BUSY_TITLE && event.getGuestList().length === 0) {
      event.deleteEvent();
      deleted++;
    }
  }

  Logger.log(`Deleted ${deleted} existing Busy events`);
  return deleted;
}

function roundUpToSlot(date) {
  const result = new Date(date);
  const minutes = result.getMinutes();

  if (minutes === 0 || minutes === 30) {
    return result;
  }

  if (minutes < 30) {
    result.setMinutes(30, 0, 0);
  } else {
    result.setMinutes(0, 0, 0);
    result.setHours(result.getHours() + 1);
  }

  return result;
}

function roundDownToSlot(date) {
  const result = new Date(date);
  const minutes = result.getMinutes();

  if (minutes < 30) {
    result.setMinutes(0, 0, 0);
  } else {
    result.setMinutes(30, 0, 0);
  }

  return result;
}

function findGaps(events, dayStart, dayEnd) {
  const gaps = [];

  const sorted = events
    .filter(e => e.getStartTime() < dayEnd && e.getEndTime() > dayStart)
    .sort((a, b) => a.getStartTime() - b.getStartTime());

  let currentTime = new Date(dayStart);

  for (const event of sorted) {
    const eventStart = event.getStartTime();
    const eventEnd = event.getEndTime();

    const clampedStart = eventStart < dayStart ? dayStart : eventStart;
    const clampedEnd = eventEnd > dayEnd ? dayEnd : eventEnd;

    if (currentTime < clampedStart) {
      gaps.push({
        start: new Date(currentTime),
        end: new Date(clampedStart)
      });
    }

    if (clampedEnd > currentTime) {
      currentTime = new Date(clampedEnd);
    }
  }

  if (currentTime < dayEnd) {
    gaps.push({
      start: new Date(currentTime),
      end: new Date(dayEnd)
    });
  }

  return gaps;
}

function getUsableGapMinutes(gap) {
  const slotStart = roundUpToSlot(gap.start);
  const slotEnd = roundDownToSlot(gap.end);
  const minutes = (slotEnd - slotStart) / (1000 * 60);
  return Math.max(0, minutes);
}

function fillGapWithBusyEvents(calendar, gap, maxMinutes) {
  const events = [];
  let slotStart = roundUpToSlot(gap.start);
  const slotEnd = roundDownToSlot(gap.end);
  let minutesFilled = 0;

  while (slotStart < slotEnd) {
    if (maxMinutes !== null && minutesFilled >= maxMinutes) {
      break;
    }

    const remainingInGap = (slotEnd - slotStart) / (1000 * 60);
    const remainingAllowed = maxMinutes !== null ? maxMinutes - minutesFilled : Infinity;

    let duration;
    if (remainingInGap >= 50 && remainingAllowed >= 50) {
      duration = 50;
    } else if (remainingInGap >= 25 && remainingAllowed >= 25) {
      duration = 25;
    } else {
      break;
    }

    const eventEnd = new Date(slotStart.getTime() + duration * 60 * 1000);

    const event = calendar.createEvent(BUSY_TITLE, slotStart, eventEnd);
    events.push(event);
    minutesFilled += duration;

    slotStart = new Date(eventEnd);

    if (slotStart.getMinutes() !== 0 && slotStart.getMinutes() !== 30) {
      slotStart = roundUpToSlot(slotStart);
    }
  }

  return { events, minutesFilled };
}

function fillBusyEvents() {
  const startTime = Date.now();
  const calendar = CalendarApp.getDefaultCalendar();
  const { startHour, endHour } = getWorkingHours();
  const { start: rangeStart, end: rangeEnd } = getDateRange();
  const now = new Date();
  const currentWeekStart = getWeekStart(now);
  const nextWeekStart = new Date(currentWeekStart);
  nextWeekStart.setDate(nextWeekStart.getDate() + 7);

  Logger.log(`Filling busy events from ${rangeStart} to ${rangeEnd}`);
  Logger.log(`Working hours: ${startHour}:00 - ${endHour}:00`);

  deleteExistingBusyEvents(calendar, rangeStart, rangeEnd);
  Utilities.sleep(1000);

  // Group days by week
  const weekDays = new Map();
  const current = new Date(rangeStart);

  while (current < rangeEnd) {
    if (isWorkingDay(current)) {
      const weekStart = getWeekStart(current).getTime();
      if (!weekDays.has(weekStart)) {
        weekDays.set(weekStart, []);
      }
      weekDays.get(weekStart).push(new Date(current));
    }
    current.setDate(current.getDate() + 1);
  }

  let totalCreated = 0;

  for (const [weekStartMs, days] of weekDays) {
    const isCurrentWeek = weekStartMs === currentWeekStart.getTime();
    const isNextWeek = weekStartMs === nextWeekStart.getTime();
    const weekLabel = new Date(weekStartMs).toDateString();

    // Collect all gaps for this week
    const weekGaps = [];

    for (const day of days) {
      const dayStart = new Date(day);
      dayStart.setHours(startHour, 0, 0, 0);

      const dayEnd = new Date(day);
      dayEnd.setHours(endHour, 0, 0, 0);

      if (dayEnd < now) {
        continue;
      }

      let effectiveStart = dayStart;
      if (day.toDateString() === now.toDateString() && now > dayStart) {
        effectiveStart = roundUpToSlot(now);
      }

      if (effectiveStart >= dayEnd) {
        continue;
      }

      const existingEvents = calendar.getEvents(dayStart, dayEnd)
        .filter(e => !e.isAllDayEvent())
        .filter(e => !(e.getTitle() === BUSY_TITLE && e.getGuestList().length === 0));

      const gaps = findGaps(existingEvents, effectiveStart, dayEnd);
      for (const gap of gaps) {
        weekGaps.push({ gap, day });
      }
    }

    // Calculate total usable gap time
    let totalGapMinutes = 0;
    for (const { gap } of weekGaps) {
      totalGapMinutes += getUsableGapMinutes(gap);
    }

    // Determine how much to fill
    let minutesToFill;
    let openMinutes;
    if (isCurrentWeek) {
      minutesToFill = totalGapMinutes;
      Logger.log(`Week of ${weekLabel} (current): filling all ${totalGapMinutes} minutes`);
    } else if (isNextWeek) {
      openMinutes = Math.min(totalGapMinutes, NEXT_WEEK_OPEN_MINUTES);
      minutesToFill = totalGapMinutes - openMinutes;
      Logger.log(`Week of ${weekLabel} (next): ${totalGapMinutes} min gaps, filling ${minutesToFill}, leaving ${openMinutes} open`);
    } else {
      openMinutes = Math.min(totalGapMinutes, LATER_WEEK_OPEN_MINUTES);
      minutesToFill = totalGapMinutes - openMinutes;
      Logger.log(`Week of ${weekLabel}: ${totalGapMinutes} min gaps, filling ${minutesToFill}, leaving ${openMinutes} open`);
    }

    // Fill gaps until we hit the limit
    let minutesFilled = 0;
    for (const { gap, day } of weekGaps) {
      if (minutesFilled >= minutesToFill) {
        break;
      }

      const remaining = minutesToFill - minutesFilled;
      const { events, minutesFilled: filled } = fillGapWithBusyEvents(calendar, gap, isCurrentWeek ? null : remaining);
      totalCreated += events.length;
      minutesFilled += filled;

      if (events.length > 0) {
        Logger.log(`  ${day.toDateString()}: created ${events.length} events (${filled} min)`);
      }
    }
  }

  const elapsed = Date.now() - startTime;
  Logger.log(`Created ${totalCreated} Busy events in ${elapsed}ms`);
  return totalCreated;
}

function createBusyFillTrigger() {
  const triggers = ScriptApp.getProjectTriggers();
  for (const trigger of triggers) {
    if (trigger.getHandlerFunction() === 'fillBusyEvents') {
      ScriptApp.deleteTrigger(trigger);
    }
  }

  ScriptApp.newTrigger('fillBusyEvents')
    .timeBased()
    .everyHours(4)
    .create();

  Logger.log('Created 4-hour trigger for fillBusyEvents');
}

function setupBusyFill() {
  createBusyFillTrigger();
  fillBusyEvents();
}

// For testing
return { getWorkingHours, isWorkingDay, getWeekStart, roundUpToSlot, roundDownToSlot, findGaps };
